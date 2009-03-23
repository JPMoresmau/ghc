/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: evacuation functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Storage.h"
#include "MBlock.h"
#include "Evac.h"
#include "GC.h"
#include "GCThread.h"
#include "GCUtils.h"
#include "Compact.h"
#include "Prelude.h"
#include "LdvProfile.h"
#include "Trace.h"

#if defined(PROF_SPIN) && defined(THREADED_RTS) && defined(PARALLEL_GC)
StgWord64 whitehole_spin = 0;
#endif

#if defined(THREADED_RTS) && !defined(PARALLEL_GC)
#define evacuate(p) evacuate1(p)
#define HEAP_ALLOCED_GC(p) HEAP_ALLOCED(p)
#endif

#if !defined(PARALLEL_GC)
#define copy_tag_nolock(p, info, src, size, stp, tag) \
        copy_tag(p, info, src, size, stp, tag)
#endif

/* Used to avoid long recursion due to selector thunks
 */
#define MAX_THUNK_SELECTOR_DEPTH 16

static void eval_thunk_selector (StgClosure **q, StgSelector * p, rtsBool);
STATIC_INLINE void evacuate_large(StgPtr p);

/* -----------------------------------------------------------------------------
   Allocate some space in which to copy an object.
   -------------------------------------------------------------------------- */

STATIC_INLINE StgPtr
alloc_for_copy (nat size, step *stp)
{
    StgPtr to;
    step_workspace *ws;

    /* Find out where we're going, using the handy "to" pointer in 
     * the step of the source object.  If it turns out we need to
     * evacuate to an older generation, adjust it here (see comment
     * by evacuate()).
     */
    if (stp < gct->evac_step) {
	if (gct->eager_promotion) {
	    stp = gct->evac_step;
	} else {
	    gct->failed_to_evac = rtsTrue;
	}
    }
    
    ws = &gct->steps[stp->abs_no];
    // this compiles to a single mem access to stp->abs_no only
    
    /* chain a new block onto the to-space for the destination step if
     * necessary.
     */
    to = ws->todo_free;
    if (to + size > ws->todo_lim) {
	to = todo_block_full(size, ws);
    }
    ws->todo_free = to + size;
    ASSERT(ws->todo_free >= ws->todo_bd->free && ws->todo_free <= ws->todo_lim);

    return to;
}

/* -----------------------------------------------------------------------------
   The evacuate() code
   -------------------------------------------------------------------------- */

STATIC_INLINE GNUC_ATTR_HOT void
copy_tag(StgClosure **p, const StgInfoTable *info, 
         StgClosure *src, nat size, step *stp, StgWord tag)
{
    StgPtr to, from;
    nat i;

    to = alloc_for_copy(size,stp);
    
    TICK_GC_WORDS_COPIED(size);

    from = (StgPtr)src;
    to[0] = (W_)info;
    for (i = 1; i < size; i++) { // unroll for small i
	to[i] = from[i];
    }

//  if (to+size+2 < bd->start + BLOCK_SIZE_W) {
//      __builtin_prefetch(to + size + 2, 1);
//  }

#if defined(PARALLEL_GC)
    {
        const StgInfoTable *new_info;
        new_info = (const StgInfoTable *)cas((StgPtr)&src->header.info, (W_)info, MK_FORWARDING_PTR(to));
        if (new_info != info) {
            return evacuate(p); // does the failed_to_evac stuff
        } else {
            *p = TAG_CLOSURE(tag,(StgClosure*)to);
        }
    }
#else
    src->header.info = (const StgInfoTable *)MK_FORWARDING_PTR(to);
    *p = TAG_CLOSURE(tag,(StgClosure*)to);
#endif

#ifdef PROFILING
    // We store the size of the just evacuated object in the LDV word so that
    // the profiler can guess the position of the next object later.
    SET_EVACUAEE_FOR_LDV(from, size);
#endif
}

#if defined(PARALLEL_GC)
STATIC_INLINE void
copy_tag_nolock(StgClosure **p, const StgInfoTable *info, 
         StgClosure *src, nat size, step *stp, StgWord tag)
{
    StgPtr to, from;
    nat i;

    to = alloc_for_copy(size,stp);
    *p = TAG_CLOSURE(tag,(StgClosure*)to);
    src->header.info = (const StgInfoTable *)MK_FORWARDING_PTR(to);
    
    TICK_GC_WORDS_COPIED(size);

    from = (StgPtr)src;
    to[0] = (W_)info;
    for (i = 1; i < size; i++) { // unroll for small i
	to[i] = from[i];
    }

//  if (to+size+2 < bd->start + BLOCK_SIZE_W) {
//      __builtin_prefetch(to + size + 2, 1);
//  }

#ifdef PROFILING
    // We store the size of the just evacuated object in the LDV word so that
    // the profiler can guess the position of the next object later.
    SET_EVACUAEE_FOR_LDV(from, size);
#endif
}
#endif

/* Special version of copy() for when we only want to copy the info
 * pointer of an object, but reserve some padding after it.  This is
 * used to optimise evacuation of BLACKHOLEs.
 */
static rtsBool
copyPart(StgClosure **p, StgClosure *src, nat size_to_reserve, nat size_to_copy, step *stp)
{
    StgPtr to, from;
    nat i;
    StgWord info;
    
#if defined(PARALLEL_GC)
spin:
	info = xchg((StgPtr)&src->header.info, (W_)&stg_WHITEHOLE_info);
	if (info == (W_)&stg_WHITEHOLE_info) {
#ifdef PROF_SPIN
	    whitehole_spin++;
#endif
	    goto spin;
	}
    if (IS_FORWARDING_PTR(info)) {
	src->header.info = (const StgInfoTable *)info;
	evacuate(p); // does the failed_to_evac stuff
	return rtsFalse;
    }
#else
    info = (W_)src->header.info;
#endif

    to = alloc_for_copy(size_to_reserve, stp);
    *p = (StgClosure *)to;

    TICK_GC_WORDS_COPIED(size_to_copy);

    from = (StgPtr)src;
    to[0] = info;
    for (i = 1; i < size_to_copy; i++) { // unroll for small i
	to[i] = from[i];
    }
    
#if defined(PARALLEL_GC)
    write_barrier();
#endif
    src->header.info = (const StgInfoTable*)MK_FORWARDING_PTR(to);
    
#ifdef PROFILING
    // We store the size of the just evacuated object in the LDV word so that
    // the profiler can guess the position of the next object later.
    SET_EVACUAEE_FOR_LDV(from, size_to_reserve);
    // fill the slop
    if (size_to_reserve - size_to_copy > 0)
	LDV_FILL_SLOP(to + size_to_copy, (int)(size_to_reserve - size_to_copy));
#endif

    return rtsTrue;
}


/* Copy wrappers that don't tag the closure after copying */
STATIC_INLINE GNUC_ATTR_HOT void
copy(StgClosure **p, const StgInfoTable *info, 
     StgClosure *src, nat size, step *stp)
{
    copy_tag(p,info,src,size,stp,0);
}

/* -----------------------------------------------------------------------------
   Evacuate a large object

   This just consists of removing the object from the (doubly-linked)
   step->large_objects list, and linking it on to the (singly-linked)
   step->new_large_objects list, from where it will be scavenged later.

   Convention: bd->flags has BF_EVACUATED set for a large object
   that has been evacuated, or unset otherwise.
   -------------------------------------------------------------------------- */

STATIC_INLINE void
evacuate_large(StgPtr p)
{
  bdescr *bd = Bdescr(p);
  step *stp, *new_stp;
  step_workspace *ws;
    
  stp = bd->step;
  ACQUIRE_SPIN_LOCK(&stp->sync_large_objects);

  // already evacuated? 
  if (bd->flags & BF_EVACUATED) { 
    /* Don't forget to set the gct->failed_to_evac flag if we didn't get
     * the desired destination (see comments in evacuate()).
     */
    if (stp < gct->evac_step) {
	gct->failed_to_evac = rtsTrue;
	TICK_GC_FAILED_PROMOTION();
    }
    RELEASE_SPIN_LOCK(&stp->sync_large_objects);
    return;
  }

  // remove from large_object list 
  if (bd->u.back) {
    bd->u.back->link = bd->link;
  } else { // first object in the list 
    stp->large_objects = bd->link;
  }
  if (bd->link) {
    bd->link->u.back = bd->u.back;
  }
  
  /* link it on to the evacuated large object list of the destination step
   */
  new_stp = stp->to;
  if (new_stp < gct->evac_step) {
      if (gct->eager_promotion) {
	  new_stp = gct->evac_step;
      } else {
	  gct->failed_to_evac = rtsTrue;
      }
  }

  ws = &gct->steps[new_stp->abs_no];

  bd->flags |= BF_EVACUATED;
  bd->step = new_stp;
  bd->gen_no = new_stp->gen_no;

  // If this is a block of pinned objects, we don't have to scan
  // these objects, because they aren't allowed to contain any
  // pointers.  For these blocks, we skip the scavenge stage and put
  // them straight on the scavenged_large_objects list.
  if (bd->flags & BF_PINNED) {
      ASSERT(get_itbl((StgClosure *)p)->type == ARR_WORDS);
      if (new_stp != stp) { ACQUIRE_SPIN_LOCK(&new_stp->sync_large_objects); }
      dbl_link_onto(bd, &new_stp->scavenged_large_objects);
      new_stp->n_scavenged_large_blocks += bd->blocks;
      if (new_stp != stp) { RELEASE_SPIN_LOCK(&new_stp->sync_large_objects); }
  } else {
      bd->link = ws->todo_large_objects;
      ws->todo_large_objects = bd;
  }

  RELEASE_SPIN_LOCK(&stp->sync_large_objects);
}

/* ----------------------------------------------------------------------------
   Evacuate

   This is called (eventually) for every live object in the system.

   The caller to evacuate specifies a desired generation in the
   gct->evac_step thread-local variable.  The following conditions apply to
   evacuating an object which resides in generation M when we're
   collecting up to generation N

   if  M >= gct->evac_step 
           if  M > N     do nothing
	   else          evac to step->to

   if  M < gct->evac_step      evac to gct->evac_step, step 0

   if the object is already evacuated, then we check which generation
   it now resides in.

   if  M >= gct->evac_step     do nothing
   if  M <  gct->evac_step     set gct->failed_to_evac flag to indicate that we
                         didn't manage to evacuate this object into gct->evac_step.


   OPTIMISATION NOTES:

   evacuate() is the single most important function performance-wise
   in the GC.  Various things have been tried to speed it up, but as
   far as I can tell the code generated by gcc 3.2 with -O2 is about
   as good as it's going to get.  We pass the argument to evacuate()
   in a register using the 'regparm' attribute (see the prototype for
   evacuate() near the top of this file).

   Changing evacuate() to take an (StgClosure **) rather than
   returning the new pointer seems attractive, because we can avoid
   writing back the pointer when it hasn't changed (eg. for a static
   object, or an object in a generation > N).  However, I tried it and
   it doesn't help.  One reason is that the (StgClosure **) pointer
   gets spilled to the stack inside evacuate(), resulting in far more
   extra reads/writes than we save.
   ------------------------------------------------------------------------- */

REGPARM1 GNUC_ATTR_HOT void 
evacuate(StgClosure **p)
{
  bdescr *bd = NULL;
  step *stp;
  StgClosure *q;
  const StgInfoTable *info;
  StgWord tag;

  q = *p;

loop:
  /* The tag and the pointer are split, to be merged after evacing */
  tag = GET_CLOSURE_TAG(q);
  q = UNTAG_CLOSURE(q);

  ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

  if (!HEAP_ALLOCED_GC(q)) {

      if (!major_gc) return;

      info = get_itbl(q);
      switch (info->type) {

      case THUNK_STATIC:
	  if (info->srt_bitmap != 0) {
	      if (*THUNK_STATIC_LINK((StgClosure *)q) == NULL) {
#ifndef THREADED_RTS
		  *THUNK_STATIC_LINK((StgClosure *)q) = gct->static_objects;
		  gct->static_objects = (StgClosure *)q;
#else
                  StgPtr link;
                  link = (StgPtr)cas((StgPtr)THUNK_STATIC_LINK((StgClosure *)q),
                                     (StgWord)NULL,
                                     (StgWord)gct->static_objects);
                  if (link == NULL) {
                      gct->static_objects = (StgClosure *)q;
                  }
#endif
	      }
	  }
	  return;

      case FUN_STATIC:
	  if (info->srt_bitmap != 0 &&
	      *FUN_STATIC_LINK((StgClosure *)q) == NULL) {
#ifndef THREADED_RTS
              *FUN_STATIC_LINK((StgClosure *)q) = gct->static_objects;
              gct->static_objects = (StgClosure *)q;
#else
              StgPtr link;
              link = (StgPtr)cas((StgPtr)FUN_STATIC_LINK((StgClosure *)q),
                                 (StgWord)NULL,
                                 (StgWord)gct->static_objects);
              if (link == NULL) {
                  gct->static_objects = (StgClosure *)q;
              }
#endif
	  }
	  return;
	  
      case IND_STATIC:
	  /* If q->saved_info != NULL, then it's a revertible CAF - it'll be
	   * on the CAF list, so don't do anything with it here (we'll
	   * scavenge it later).
	   */
	  if (((StgIndStatic *)q)->saved_info == NULL) {
	      if (*IND_STATIC_LINK((StgClosure *)q) == NULL) {
#ifndef THREADED_RTS
		  *IND_STATIC_LINK((StgClosure *)q) = gct->static_objects;
		  gct->static_objects = (StgClosure *)q;
#else
                  StgPtr link;
                  link = (StgPtr)cas((StgPtr)IND_STATIC_LINK((StgClosure *)q),
                                     (StgWord)NULL,
                                     (StgWord)gct->static_objects);
                  if (link == NULL) {
                      gct->static_objects = (StgClosure *)q;
                  }
#endif
	      }
	  }
	  return;
	  
      case CONSTR_STATIC:
	  if (*STATIC_LINK(info,(StgClosure *)q) == NULL) {
#ifndef THREADED_RTS
              *STATIC_LINK(info,(StgClosure *)q) = gct->static_objects;
              gct->static_objects = (StgClosure *)q;
#else
              StgPtr link;
              link = (StgPtr)cas((StgPtr)STATIC_LINK(info,(StgClosure *)q),
                                 (StgWord)NULL,
                                 (StgWord)gct->static_objects);
              if (link == NULL) {
                  gct->static_objects = (StgClosure *)q;
              }
#endif
          }
          /* I am assuming that static_objects pointers are not
           * written to other objects, and thus, no need to retag. */
          return;
	  
      case CONSTR_NOCAF_STATIC:
	  /* no need to put these on the static linked list, they don't need
	   * to be scavenged.
	   */
	  return;
	  
      default:
	  barf("evacuate(static): strange closure type %d", (int)(info->type));
      }
  }

  bd = Bdescr((P_)q);

  if ((bd->flags & (BF_LARGE | BF_MARKED | BF_EVACUATED)) != 0) {

      // pointer into to-space: just return it.  It might be a pointer
      // into a generation that we aren't collecting (> N), or it
      // might just be a pointer into to-space.  The latter doesn't
      // happen often, but allowing it makes certain things a bit
      // easier; e.g. scavenging an object is idempotent, so it's OK to
      // have an object on the mutable list multiple times.
      if (bd->flags & BF_EVACUATED) {
          // We aren't copying this object, so we have to check
          // whether it is already in the target generation.  (this is
          // the write barrier).
	  if (bd->step < gct->evac_step) {
	      gct->failed_to_evac = rtsTrue;
	      TICK_GC_FAILED_PROMOTION();
	  }
	  return;
      }

      /* evacuate large objects by re-linking them onto a different list.
       */
      if (bd->flags & BF_LARGE) {
	  info = get_itbl(q);
	  if (info->type == TSO && 
	      ((StgTSO *)q)->what_next == ThreadRelocated) {
	      q = (StgClosure *)((StgTSO *)q)->_link;
              *p = q;
	      goto loop;
	  }
	  evacuate_large((P_)q);
	  return;
      }
      
      /* If the object is in a step that we're compacting, then we
       * need to use an alternative evacuate procedure.
       */
      if (!is_marked((P_)q,bd)) {
          mark((P_)q,bd);
          if (mark_stack_full()) {
              debugTrace(DEBUG_gc,"mark stack overflowed");
              mark_stack_overflowed = rtsTrue;
              reset_mark_stack();
          }
          push_mark_stack((P_)q);
      }
      return;
  }
      
  stp = bd->step->to;

  info = q->header.info;
  if (IS_FORWARDING_PTR(info))
  {
    /* Already evacuated, just return the forwarding address.
     * HOWEVER: if the requested destination generation (gct->evac_step) is
     * older than the actual generation (because the object was
     * already evacuated to a younger generation) then we have to
     * set the gct->failed_to_evac flag to indicate that we couldn't 
     * manage to promote the object to the desired generation.
     */
    /* 
     * Optimisation: the check is fairly expensive, but we can often
     * shortcut it if either the required generation is 0, or the
     * current object (the EVACUATED) is in a high enough generation.
     * We know that an EVACUATED always points to an object in the
     * same or an older generation.  stp is the lowest step that the
     * current object would be evacuated to, so we only do the full
     * check if stp is too low.
     */
      StgClosure *e = (StgClosure*)UN_FORWARDING_PTR(info);
      *p = TAG_CLOSURE(tag,e);
      if (stp < gct->evac_step) {  // optimisation 
	  if (Bdescr((P_)e)->step < gct->evac_step) {
	      gct->failed_to_evac = rtsTrue;
	      TICK_GC_FAILED_PROMOTION();
	  }
      }
      return;
  }

  switch (INFO_PTR_TO_STRUCT(info)->type) {

  case WHITEHOLE:
      goto loop;

  case MUT_VAR_CLEAN:
  case MUT_VAR_DIRTY:
  case MVAR_CLEAN:
  case MVAR_DIRTY:
      copy(p,info,q,sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)),stp);
      return;

  case CONSTR_0_1:
  { 
      StgWord w = (StgWord)q->payload[0];
      if (info == Czh_con_info &&
	  // unsigned, so always true:  (StgChar)w >= MIN_CHARLIKE &&  
	  (StgChar)w <= MAX_CHARLIKE) {
	  *p =  TAG_CLOSURE(tag,
                            (StgClosure *)CHARLIKE_CLOSURE((StgChar)w)
			   );
      }
      else if (info == Izh_con_info &&
	  (StgInt)w >= MIN_INTLIKE && (StgInt)w <= MAX_INTLIKE) {
	  *p = TAG_CLOSURE(tag,
			     (StgClosure *)INTLIKE_CLOSURE((StgInt)w)
			     );
      }
      else {
          copy_tag_nolock(p,info,q,sizeofW(StgHeader)+1,stp,tag);
      }
      return;
  }

  case FUN_0_1:
  case FUN_1_0:
  case CONSTR_1_0:
      copy_tag_nolock(p,info,q,sizeofW(StgHeader)+1,stp,tag);
      return;

  case THUNK_1_0:
  case THUNK_0_1:
      copy(p,info,q,sizeofW(StgThunk)+1,stp);
      return;

  case THUNK_1_1:
  case THUNK_2_0:
  case THUNK_0_2:
#ifdef NO_PROMOTE_THUNKS
    if (bd->gen_no == 0 && 
	bd->step->no != 0 &&
	bd->step->no == generations[bd->gen_no].n_steps-1) {
      stp = bd->step;
    }
#endif
    copy(p,info,q,sizeofW(StgThunk)+2,stp);
    return;

  case FUN_1_1:
  case FUN_2_0:
  case FUN_0_2:
  case CONSTR_1_1:
  case CONSTR_2_0:
      copy_tag_nolock(p,info,q,sizeofW(StgHeader)+2,stp,tag);
      return;

  case CONSTR_0_2:
      copy_tag_nolock(p,info,q,sizeofW(StgHeader)+2,stp,tag);
      return;

  case THUNK:
      copy(p,info,q,thunk_sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)),stp);
      return;

  case FUN:
  case IND_PERM:
  case IND_OLDGEN_PERM:
  case CONSTR:
      copy_tag_nolock(p,info,q,sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)),stp,tag);
      return;

  case WEAK:
  case STABLE_NAME:
      copy_tag(p,info,q,sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)),stp,tag);
      return;

  case BCO:
      copy(p,info,q,bco_sizeW((StgBCO *)q),stp);
      return;

  case CAF_BLACKHOLE:
  case BLACKHOLE:
      copyPart(p,q,BLACKHOLE_sizeW(),sizeofW(StgHeader),stp);
      return;

  case THUNK_SELECTOR:
      eval_thunk_selector(p, (StgSelector *)q, rtsTrue);
      return;

  case IND:
  case IND_OLDGEN:
    // follow chains of indirections, don't evacuate them 
    q = ((StgInd*)q)->indirectee;
    *p = q;
    goto loop;

  case RET_BCO:
  case RET_SMALL:
  case RET_BIG:
  case RET_DYN:
  case UPDATE_FRAME:
  case STOP_FRAME:
  case CATCH_FRAME:
  case CATCH_STM_FRAME:
  case CATCH_RETRY_FRAME:
  case ATOMICALLY_FRAME:
    // shouldn't see these 
    barf("evacuate: stack frame at %p\n", q);

  case PAP:
      copy(p,info,q,pap_sizeW((StgPAP*)q),stp);
      return;

  case AP:
      copy(p,info,q,ap_sizeW((StgAP*)q),stp);
      return;

  case AP_STACK:
      copy(p,info,q,ap_stack_sizeW((StgAP_STACK*)q),stp);
      return;

  case ARR_WORDS:
      // just copy the block 
      copy(p,info,q,arr_words_sizeW((StgArrWords *)q),stp);
      return;

  case MUT_ARR_PTRS_CLEAN:
  case MUT_ARR_PTRS_DIRTY:
  case MUT_ARR_PTRS_FROZEN:
  case MUT_ARR_PTRS_FROZEN0:
      // just copy the block 
      copy(p,info,q,mut_arr_ptrs_sizeW((StgMutArrPtrs *)q),stp);
      return;

  case TSO:
    {
      StgTSO *tso = (StgTSO *)q;

      /* Deal with redirected TSOs (a TSO that's had its stack enlarged).
       */
      if (tso->what_next == ThreadRelocated) {
	q = (StgClosure *)tso->_link;
	*p = q;
	goto loop;
      }

      /* To evacuate a small TSO, we need to relocate the update frame
       * list it contains.  
       */
      {
	  StgTSO *new_tso;
	  StgPtr r, s;
          rtsBool mine;

	  mine = copyPart(p,(StgClosure *)tso, tso_sizeW(tso), 
                          sizeofW(StgTSO), stp);
          if (mine) {
              new_tso = (StgTSO *)*p;
              move_TSO(tso, new_tso);
              for (r = tso->sp, s = new_tso->sp;
                   r < tso->stack+tso->stack_size;) {
                  *s++ = *r++;
              }
          }
	  return;
      }
    }

  case TREC_HEADER: 
      copy(p,info,q,sizeofW(StgTRecHeader),stp);
      return;

  case TVAR_WATCH_QUEUE:
      copy(p,info,q,sizeofW(StgTVarWatchQueue),stp);
      return;

  case TVAR:
      copy(p,info,q,sizeofW(StgTVar),stp);
      return;
    
  case TREC_CHUNK:
      copy(p,info,q,sizeofW(StgTRecChunk),stp);
      return;

  case ATOMIC_INVARIANT:
      copy(p,info,q,sizeofW(StgAtomicInvariant),stp);
      return;

  case INVARIANT_CHECK_QUEUE:
      copy(p,info,q,sizeofW(StgInvariantCheckQueue),stp);
      return;

  default:
    barf("evacuate: strange closure type %d", (int)(INFO_PTR_TO_STRUCT(info)->type));
  }

  barf("evacuate");
}

/* -----------------------------------------------------------------------------
   Evaluate a THUNK_SELECTOR if possible.

   p points to a THUNK_SELECTOR that we want to evaluate.  The
   result of "evaluating" it will be evacuated and a pointer to the
   to-space closure will be returned.

   If the THUNK_SELECTOR could not be evaluated (its selectee is still
   a THUNK, for example), then the THUNK_SELECTOR itself will be
   evacuated.
   -------------------------------------------------------------------------- */
static void
unchain_thunk_selectors(StgSelector *p, StgClosure *val)
{
    StgSelector *prev;

    prev = NULL;
    while (p)
    {
#ifdef THREADED_RTS
        ASSERT(p->header.info == &stg_WHITEHOLE_info);
#else
        ASSERT(p->header.info == &stg_BLACKHOLE_info);
#endif
        // val must be in to-space.  Not always: when we recursively
        // invoke eval_thunk_selector(), the recursive calls will not 
        // evacuate the value (because we want to select on the value,
        // not evacuate it), so in this case val is in from-space.
        // ASSERT(!HEAP_ALLOCED_GC(val) || Bdescr((P_)val)->gen_no > N || (Bdescr((P_)val)->flags & BF_EVACUATED));

        prev = (StgSelector*)((StgClosure *)p)->payload[0];

        // Update the THUNK_SELECTOR with an indirection to the
        // value.  The value is still in from-space at this stage.
        //
        // (old note: Why not do upd_evacuee(q,p)?  Because we have an
        // invariant that an EVACUATED closure always points to an
        // object in the same or an older generation (required by
        // the short-cut test in the EVACUATED case, below).
        if ((StgClosure *)p == val) {
            // must be a loop; just leave a BLACKHOLE in place.  This
            // can happen when we have a chain of selectors that
            // eventually loops back on itself.  We can't leave an
            // indirection pointing to itself, and we want the program
            // to deadlock if it ever enters this closure, so
            // BLACKHOLE is correct.
            SET_INFO(p, &stg_BLACKHOLE_info);
        } else {
            ((StgInd *)p)->indirectee = val;
            write_barrier();
            SET_INFO(p, &stg_IND_info);
        }

        // For the purposes of LDV profiling, we have created an
        // indirection.
        LDV_RECORD_CREATE(p);

        p = prev;
    }
}

static void
eval_thunk_selector (StgClosure **q, StgSelector * p, rtsBool evac)
                 // NB. for legacy reasons, p & q are swapped around :(
{
    nat field;
    StgInfoTable *info;
    StgWord info_ptr;
    StgClosure *selectee;
    StgSelector *prev_thunk_selector;
    bdescr *bd;
    StgClosure *val;
    
    prev_thunk_selector = NULL;
    // this is a chain of THUNK_SELECTORs that we are going to update
    // to point to the value of the current THUNK_SELECTOR.  Each
    // closure on the chain is a BLACKHOLE, and points to the next in the
    // chain with payload[0].

selector_chain:

    bd = Bdescr((StgPtr)p);
    if (HEAP_ALLOCED_GC(p)) {
        // If the THUNK_SELECTOR is in to-space or in a generation that we
        // are not collecting, then bale out early.  We won't be able to
        // save any space in any case, and updating with an indirection is
        // trickier in a non-collected gen: we would have to update the
        // mutable list.
        if (bd->flags & BF_EVACUATED) {
            unchain_thunk_selectors(prev_thunk_selector, (StgClosure *)p);
            *q = (StgClosure *)p;
            // shortcut, behave as for:  if (evac) evacuate(q);
            if (evac && bd->step < gct->evac_step) {
                gct->failed_to_evac = rtsTrue;
                TICK_GC_FAILED_PROMOTION();
            }
            return;
        }
        // we don't update THUNK_SELECTORS in the compacted
        // generation, because compaction does not remove the INDs
        // that result, this causes confusion later
        // (scavenge_mark_stack doesn't deal with IND).  BEWARE!  This
        // bit is very tricky to get right.  If you make changes
        // around here, test by compiling stage 3 with +RTS -c -RTS.
        if (bd->flags & BF_MARKED) {
            // must call evacuate() to mark this closure if evac==rtsTrue
            *q = (StgClosure *)p;
            if (evac) evacuate(q);
            unchain_thunk_selectors(prev_thunk_selector, (StgClosure *)p);
            return;
        }
    }


    // BLACKHOLE the selector thunk, since it is now under evaluation.
    // This is important to stop us going into an infinite loop if
    // this selector thunk eventually refers to itself.
#if defined(THREADED_RTS)
    // In threaded mode, we'll use WHITEHOLE to lock the selector
    // thunk while we evaluate it.
    {
        do {
            info_ptr = xchg((StgPtr)&p->header.info, (W_)&stg_WHITEHOLE_info);
        } while (info_ptr == (W_)&stg_WHITEHOLE_info);

        // make sure someone else didn't get here first...
        if (IS_FORWARDING_PTR(p) || 
            INFO_PTR_TO_STRUCT(info_ptr)->type != THUNK_SELECTOR) {
            // v. tricky now.  The THUNK_SELECTOR has been evacuated
            // by another thread, and is now either a forwarding ptr or IND.
            // We need to extract ourselves from the current situation
            // as cleanly as possible.
            //   - unlock the closure
            //   - update *q, we may have done *some* evaluation
            //   - if evac, we need to call evacuate(), because we
            //     need the write-barrier stuff.
            //   - undo the chain we've built to point to p.
            SET_INFO(p, (const StgInfoTable *)info_ptr);
            *q = (StgClosure *)p;
            if (evac) evacuate(q);
            unchain_thunk_selectors(prev_thunk_selector, (StgClosure *)p);
            return;
        }
    }
#else
    // Save the real info pointer (NOTE: not the same as get_itbl()).
    info_ptr = (StgWord)p->header.info;
    SET_INFO(p,&stg_BLACKHOLE_info);
#endif

    field = INFO_PTR_TO_STRUCT(info_ptr)->layout.selector_offset;

    // The selectee might be a constructor closure,
    // so we untag the pointer.
    selectee = UNTAG_CLOSURE(p->selectee);

selector_loop:
    // selectee now points to the closure that we're trying to select
    // a field from.  It may or may not be in to-space: we try not to
    // end up in to-space, but it's impractical to avoid it in
    // general.  The compacting GC scatters to-space pointers in
    // from-space during marking, for example.  We rely on the property
    // that evacuate() doesn't mind if it gets passed a to-space pointer.

    info = (StgInfoTable*)selectee->header.info;

    if (IS_FORWARDING_PTR(info)) {
        // We don't follow pointers into to-space; the constructor
        // has already been evacuated, so we won't save any space
        // leaks by evaluating this selector thunk anyhow.
        goto bale_out;
    }

    info = INFO_PTR_TO_STRUCT(info);
    switch (info->type) {
      case WHITEHOLE:
	  goto bale_out; // about to be evacuated by another thread (or a loop).
	
      case CONSTR:
      case CONSTR_1_0:
      case CONSTR_0_1:
      case CONSTR_2_0:
      case CONSTR_1_1:
      case CONSTR_0_2:
      case CONSTR_STATIC:
      case CONSTR_NOCAF_STATIC:
          {
              // check that the size is in range 
              ASSERT(field <  (StgWord32)(info->layout.payload.ptrs + 
                                          info->layout.payload.nptrs));
	  
              // Select the right field from the constructor
              val = selectee->payload[field];
              
#ifdef PROFILING
              // For the purposes of LDV profiling, we have destroyed
              // the original selector thunk, p.
              SET_INFO(p, (StgInfoTable *)info_ptr);
              LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC((StgClosure *)p);
#if defined(THREADED_RTS)
              SET_INFO(p, &stg_WHITEHOLE_info);
#else
              SET_INFO(p, &stg_BLACKHOLE_info);
#endif
#endif

              // the closure in val is now the "value" of the
              // THUNK_SELECTOR in p.  However, val may itself be a
              // THUNK_SELECTOR, in which case we want to continue
              // evaluating until we find the real value, and then
              // update the whole chain to point to the value.
          val_loop:
              info_ptr = (StgWord)UNTAG_CLOSURE(val)->header.info;
              if (!IS_FORWARDING_PTR(info_ptr))
              {
                  info = INFO_PTR_TO_STRUCT(info_ptr);
                  switch (info->type) {
                  case IND:
                  case IND_PERM:
                  case IND_OLDGEN:
                  case IND_OLDGEN_PERM:
                  case IND_STATIC:
                      val = ((StgInd *)val)->indirectee;
                      goto val_loop;
                  case THUNK_SELECTOR:
                      ((StgClosure*)p)->payload[0] = (StgClosure *)prev_thunk_selector;
                      prev_thunk_selector = p;
                      p = (StgSelector*)val;
                      goto selector_chain;
                  default:
                      break;
                  }
              }
              ((StgClosure*)p)->payload[0] = (StgClosure *)prev_thunk_selector;
              prev_thunk_selector = p;

              *q = val;

              // update the other selectors in the chain *before*
              // evacuating the value.  This is necessary in the case
              // where the value turns out to be one of the selectors
              // in the chain (i.e. we have a loop), and evacuating it
              // would corrupt the chain.
              unchain_thunk_selectors(prev_thunk_selector, val);

              // evacuate() cannot recurse through
              // eval_thunk_selector(), because we know val is not
              // a THUNK_SELECTOR.
              if (evac) evacuate(q);
              return;
          }

      case IND:
      case IND_PERM:
      case IND_OLDGEN:
      case IND_OLDGEN_PERM:
      case IND_STATIC:
          // Again, we might need to untag a constructor.
          selectee = UNTAG_CLOSURE( ((StgInd *)selectee)->indirectee );
	  goto selector_loop;

      case THUNK_SELECTOR:
      {
	  StgClosure *val;

          // recursively evaluate this selector.  We don't want to
          // recurse indefinitely, so we impose a depth bound.
	  if (gct->thunk_selector_depth >= MAX_THUNK_SELECTOR_DEPTH) {
	      goto bale_out;
	  }

	  gct->thunk_selector_depth++;
          // rtsFalse says "don't evacuate the result".  It will,
          // however, update any THUNK_SELECTORs that are evaluated
          // along the way.
	  eval_thunk_selector(&val, (StgSelector*)selectee, rtsFalse);
	  gct->thunk_selector_depth--;

          // did we actually manage to evaluate it?
          if (val == selectee) goto bale_out;

          // Of course this pointer might be tagged...
          selectee = UNTAG_CLOSURE(val);
          goto selector_loop;
      }

      case AP:
      case AP_STACK:
      case THUNK:
      case THUNK_1_0:
      case THUNK_0_1:
      case THUNK_2_0:
      case THUNK_1_1:
      case THUNK_0_2:
      case THUNK_STATIC:
      case CAF_BLACKHOLE:
      case BLACKHOLE:
	  // not evaluated yet 
	  goto bale_out;
    
      default:
	barf("eval_thunk_selector: strange selectee %d",
	     (int)(info->type));
    }

bale_out:
    // We didn't manage to evaluate this thunk; restore the old info
    // pointer.  But don't forget: we still need to evacuate the thunk itself.
    SET_INFO(p, (const StgInfoTable *)info_ptr);
    // THREADED_RTS: we just unlocked the thunk, so another thread
    // might get in and update it.  copy() will lock it again and
    // check whether it was updated in the meantime.
    *q = (StgClosure *)p;
    if (evac) {
        copy(q,(const StgInfoTable *)info_ptr,(StgClosure *)p,THUNK_SELECTOR_sizeW(),bd->step->to);
    }
    unchain_thunk_selectors(prev_thunk_selector, *q);
    return;
}
