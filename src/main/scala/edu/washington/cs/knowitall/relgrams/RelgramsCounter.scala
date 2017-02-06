package edu.washington.cs.knowitall.relgrams

import utils.MapUtils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 2/25/13
 * Time: 3:31 PM
 * To change this template use File | Settings | File Templates.
 */
class RelgramsCounter(maxSize:Int) {

  def combineTupleCounts(a: RelationTupleCounts, b: RelationTupleCounts):RelationTupleCounts = reduceTupleCounts(a::b::Nil).get
  def combineRelgramCounts(a: RelgramCounts, b: RelgramCounts):RelgramCounts = reduce(a::b::Nil)

  def haveDisjointHashes(mergeWith: RelgramCounts, toMerge: RelgramCounts): Boolean = {
    val a1 = mergeWith.relgram.first.hashes
    val a2 = mergeWith.relgram.second.hashes
    val b1 = toMerge.relgram.first.hashes
    val b2 = toMerge.relgram.second.hashes
    !(a1 exists b1) && !(a2 exists b2)
  }

  def updateHashes(mergeWith: RelgramCounts, toMerge: RelgramCounts){

    if (mergeWith.relgram.first.hashes.size < 100) mergeWith.relgram.first.hashes ++= toMerge.relgram.first.hashes
    if (mergeWith.relgram.second.hashes.size < 100) mergeWith.relgram.second.hashes ++= toMerge.relgram.second.hashes

  }

  def mergeArgCounts(mergeWith: RelgramCounts, toMerge: RelgramCounts) {

    MapUtils.addToUntilSize(mergeWith.argCounts.firstArg1Counts, toMerge.argCounts.firstArg1Counts, maxSize)
    MapUtils.addToUntilSize(mergeWith.argCounts.firstArg2Counts, toMerge.argCounts.firstArg2Counts, maxSize)

    MapUtils.addToUntilSize(mergeWith.argCounts.secondArg1Counts, toMerge.argCounts.secondArg1Counts, maxSize)
    MapUtils.addToUntilSize(mergeWith.argCounts.secondArg2Counts, toMerge.argCounts.secondArg2Counts, maxSize)
  }

  def mergeCounts(mergeWith: RelgramCounts, toMerge: RelgramCounts){
    MapUtils.addTo(mergeWith.counts, toMerge.counts)
  }

  def mergeIds(mergeWith: RelgramCounts, toMerge: RelgramCounts, maxNumberOfIds:Int){
    if (mergeWith.relgram.first.ids.size < maxNumberOfIds){
      mergeWith.relgram.first.ids ++= toMerge.relgram.first.ids
    }
    if (mergeWith.relgram.second.ids.size < maxNumberOfIds){
      mergeWith.relgram.second.ids ++= toMerge.relgram.second.ids
    }
  }

  def merge(mergeWith: RelgramCounts, toMerge: RelgramCounts){
    if(haveDisjointHashes(mergeWith, toMerge)){
      updateHashes(mergeWith, toMerge)
      mergeArgCounts(mergeWith, toMerge)
      mergeCounts(mergeWith, toMerge)
      mergeIds(mergeWith, toMerge, maxSize)
    }
  }

  def reduceRelgramCounts(rgcs:Iterable[RelgramCounts], minDirFreq:Int, skipHashes:Boolean = false, skipSentences:Boolean = false): Option[RelgramCounts] = {
    val outRGC: RelgramCounts = reduce(rgcs)
    if (skipHashes){
      outRGC.relgram.first.hashes = Set[Int]()
      outRGC.relgram.second.hashes = Set[Int]()
    }
    if (skipSentences){
      outRGC.relgram.first.sentences = Set[String]()
      outRGC.relgram.second.sentences = Set[String]()
    }

    def sumOrElse(counts:Iterable[Int], orElse:Int) = if (!counts.isEmpty) outRGC.counts.values.sum else 0
    def aboveThreshold(rgc:RelgramCounts) = sumOrElse(rgc.counts.values, 0) > minDirFreq

    if(outRGC != null && aboveThreshold(outRGC)) Some(outRGC) else None
  }


  def reduce(rgcs: Iterable[RelgramCounts]): RelgramCounts = {

    var outRGC: RelgramCounts = null

    rgcs.filter(rgc => !RelgramCounts.isDummy(rgc))
        .foreach(rgc => {
      if (outRGC == null) {
        outRGC = rgc
      } else {
        merge(outRGC, rgc)
      }
    })
    outRGC
  }

  def isDummyTuple(tuple: RelationTuple) = tuple.arg1.equals("NA")


  def mergeTuple(mergedWith: RelationTuple, toMerge: RelationTuple){
    MapUtils.addToUntilSize(mergedWith.arg1HeadCounts, toMerge.arg1HeadCounts, maxSize)
    MapUtils.addToUntilSize(mergedWith.arg2HeadCounts, toMerge.arg2HeadCounts, maxSize)
  }


  def reduceTuples(tuples: Iterable[RelationTuple]) = {
    var seq = tuples
    var outTuple:RelationTuple = null
    var count = 0
    seq.filter(tuple => !isDummyTuple(tuple)).foreach(tuple =>{
      if (outTuple == null)
        outTuple = tuple
      else
        mergeTuple(outTuple, tuple)
      count = count + 1
    })
    if (outTuple != null){
      Some(new RelationTupleCounts(outTuple, count))
    }else{
      None
    }
  }

  def reduceTupleCounts(tuples: Iterable[RelationTupleCounts]) = {
    var seq = tuples
    var outTuple:RelationTupleCounts = null
    var count = 0
    seq.filter(tuple => !isDummyTuple(tuple.tuple)).foreach(tuple =>{
      if (outTuple == null)
        outTuple = tuple
      else
        mergeTupleCounts(outTuple, tuple)
      count = count + 1
    })
    Some(outTuple)

  }


  def mergeTupleCounts(mergeWith: RelationTupleCounts, toMerge: RelationTupleCounts) = {
    mergeWith.count += toMerge.count
    mergeTuple(mergeWith.tuple, toMerge.tuple)
  }



}
