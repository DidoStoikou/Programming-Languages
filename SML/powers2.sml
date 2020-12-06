

fun powers2(filename) =
  let
  fun binary (bin, 0, k, sum1) =  (bin, Int.toLarge(sum1), k , Int.toLarge(Array.sub(bin,0)))
    | binary (bin, N, k, sum1) =
      let
        fun help1 (bin, N, k) =
          let
            val _ = Array.update(bin, k, N mod 2)
          in
            bin
          end
        fun help2 (sum1, N) =
          if (N mod 2) = 1 then sum1 + 1 else sum1
      in
        binary(help1(bin, N, k), N div 2, k+1, help2(sum1, N))
      end

  fun fastpower (x, 0) = 1
  | fastpower (x, n) =
    let
      val (x1,y1) = IntInf.divMod(n,2)
    in
      if y1 = 0 then fastpower (x*x, x1)
              else x * fastpower (x*x, x1)
    end

  fun stupid(place,bin,Large) =
  if place = 1 then (bin,Large + 2)
  else
    let
      val _ = Array.update(bin, place - 1, Array.sub(bin,place - 1) + 2)
    in
      (bin,Large)
    end

  fun explore (bin, sum1, place, K, Large) =
    if Int.toLarge(K) = sum1 then (sum1, bin, Large)
    else if (Int.toLarge(K) - sum1) >= fastpower(2, Int.toLarge(place)) then
      let
        val Large1 = fastpower(2,Int.toLarge(place)) + Large
        val _ = Array.update(bin, place, Array.sub(bin,place) - 1)
      in
        (sum1 + fastpower(2,Int.toLarge(place)) -1, bin,Large1)
      end
    else
      let
        val _ = Array.update(bin, place, Array.sub(bin,place) - 1)
        val (bin1,Large1) = stupid(place,bin,Large)
        val (sumx,binx,Largex) = explore(bin1, sum1 +1, place-1, K, Large1)
        val (sumy,biny,Largey) = explore(binx, sumx, place-1, K, Largex)
      in
        (sumy, biny,Largey)
      end

  fun neasynarthsh(bin,msb,counter,j,Large) =
    if j=0 then
      let
        val _ = print("[" ^ IntInf.toString(Large) ^ ",")
      in
        neasynarthsh(bin,msb,counter,j+1,Large)
      end
    else if j < counter then
      let
        val _ = print(Int.toString(Array.sub(bin,j)) ^ ",")
      in
        neasynarthsh(bin,msb,counter,j+1,Large)
      end
    else print(Int.toString(Array.sub(bin,j)) ^ "]\n")

  fun myprint (bin, msb,Large) =
    let
      fun help3 (msb, bin, counter, j) =
        if j>msb then counter
        else if Array.sub(bin, j) <> 0 then help3(msb, bin, j, j+1)
        else help3(msb, bin, counter, j+1)
      val counter = help3(msb, bin, 0, 0)
    in
      neasynarthsh(bin,msb,counter,0,Large)
    end

  fun mywhile(sum1, K, place, bin, k, Large) =
    if (sum1 = Int.toLarge(K)) then myprint(bin, k-1,Large)
    else if Array.sub(bin, place) <> 1 then mywhile(sum1, K, place+1, bin, k, Large)
    else
      let
        val (sumx,binx,Large) = explore(bin, sum1, place, K, Large)
      in
        mywhile(sumx, K, place, binx, k, Large)
      end

  fun main (N, K, (bin, sum1, k, Large)) =
    if (Int.toLarge(K) < sum1) orelse (K > N) then print("[]\n")
    else if (Int.toLarge(K) = sum1) then myprint(bin, k-1, Large)
    else if (K = N) then print ("[" ^ Int.toString(N) ^ "]\n")
    else mywhile(sum1, K, 1, bin, k, Large)

  fun solve (0,numbers) = ()
    | solve (T,numbers) =
      let
        val N = hd(hd(numbers))
        val K = hd(tl(hd(numbers)))
        val tail = tl(numbers)
        val x = main(N,K,binary(Array.array(32, 0), N, 0, 0))
      in
        solve (T-1 , tail)
      end

    fun parse file =
      let
        fun readInt input =
          Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

        val inStream = TextIO.openIn file
        val T = readInt inStream
  	    val _ = TextIO.inputLine inStream

        fun myread inStream =
          let
            val N = readInt inStream
            val K = readInt inStream
            val _ = TextIO.inputLine inStream
          in
            [N, K]
          end

        fun readInts 0 acc = rev acc
  	       | readInts i acc = readInts (i - 1) (myread inStream :: acc)
      in
        (T, readInts T [])
      end
  in
    solve(parse (filename))
  end
