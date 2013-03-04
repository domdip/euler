scala> (1 to 100000).toList
res0: List[Int] = List(1, 2, 3, ...
 
scala> (1 to 100000).toSeq
res1: scala.collection.immutable.Range = Range(1, 2, 3, ...
 
scala> new testing.Benchmark() { override def run() = (res0.size-1 to 0 by
-100).foreach(i=>res0(i)) }
res2: java.lang.Object with scala.testing.Benchmark = $anon$1@38ddab20
 
scala> new testing.Benchmark() { override def run() = (res1.size-1 to 0 by
-100).foreach(i=>res1(i)) }
res3: java.lang.Object with scala.testing.Benchmark = $anon$1@4df4a05d
 
scala> res2.runBenchmark(2)
res4: List[Long] = List(140, 138)
 
scala> res3.runBenchmark(2)
res5: List[Long] = List(1, 1)


====

val listObj = (1 to 100000).toList

val seqObj = (1 to 100000).toList.toSeq

val listBench = new testing.Benchmark() { override def run() = (listObj.size-1 to 0 by
-100).foreach(i=>listObj(i)) }

val seqBench = new testing.Benchmark() { override def run() = (seqObj.size-1 to 0 by
-100).foreach(i=>seqObj(i)) }

println(listBench.runBenchmark(2))

println(seqBench.runBenchmark(2))