//1
def lastElement(x:List[Int]):Int={
  x match{
    case Nil => throw new NoSuchElementException
    case a::Nil => return a
    case a::b =>  return lastElement(b)
  }
}
assert ( lastElement ( List ( 1,2,3 ) ) == 3 )
assert ( lastElement ( List ( 1 ) ) == 1 )

//2
def penultimate(x:List[Int]):Int={
  x match{
    case Nil => throw new NoSuchElementException
    case a::Nil => throw new NoSuchElementException
    case a::b::Nil => a
    case a::b =>  penultimate(b)
  }
}
assert( penultimate( List( 1,2,3,4 ) ) == 3)
assert( penultimate( List( 1,2 ) ) == 1)

//assert(penultimate(List(1,2,3,4))==3)
//3
def reverse(x:List[Int]):List[Int]={
  x match{
    case Nil => Nil
    case a::Nil => List(a)
    case a::b => reverse(b):::List(a)
  }
}
assert( reverse( List( 1,2,3,4,5 ) ) ==   List(5,4,3,2,1) )
assert( reverse( List( 1 ) ) == List(1) )

//4
def compress(x:List[Int]): List[Int]={
  x match{
    case Nil => Nil
    case a::Nil=> List(a)
    case a::b::tail => if(a==b) compress(List(b):::tail)
    else List(a):::compress(List(b):::tail)
  }
}
assert( compress(List(1,1,2,2)) == List(1,2))
assert( compress(List(1,2,2,3,3,1,4,4)) == List(1,2,3,1,4))

//5
def duplicateElement(n:Int,a:Int):List[Int]= {
  n match {
    case 0 => Nil
    case n => List(a) ::: duplicateElement(n - 1, a)
  }
}
def duplicate(n:Int,x:List[Int]): List[Int]={
  (n,x) match{
    case(_,Nil)=>Nil
    case(n,a::b)=> duplicateElement(n,a):::duplicate(n,b)
  }
}
assert( duplicate (3,List(1,1)) == List(1,1,1,1,1,1))
assert( duplicate (0,List(1,2,3)) == List())
assert( duplicate (1,List(1,2,3)) == List(1,2,3))

//6
def delete(n:Int,i:Int,x:List[Int]):List[Int]={
  (n,i,x) match {
    case(0,_,_) => throw new NoSuchElementException
    case (_,_,Nil)=>Nil
    case (n,1,a::b)  => delete(n,n,b)
    case (n,i,a::b) => List(a)::: delete(n,i-1,b)
  }
}
def drop(n:Int,x:List[Int]): List[Int]={
  delete(n,n,x)
}
assert(drop(1,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == List())
assert(drop(2,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == List(1, 3, 5, 7, 9))

//7
def slice(j: Int,k: Int,a: List[Int]): List[Int] = {
  (j, k, a) match {
    case (_, _, Nil) => Nil
    case (_, 0, _) => Nil
    case (0, _, a :: b) => List(a) ::: slice(0, k - 1, b)
    case (j, k, a :: b) => slice(j - 1, k - 1, b)
  }
}
assert(slice(0,5,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == List(1, 2, 3, 4, 5))
assert(slice(1,10,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == List(2, 3, 4, 5, 6, 7, 8, 9, 10))


//8
def isPrime(j: Int, k: Int): Boolean =
  (j,k) match{
    case (_,0) => false
    case (_,1) => true
    case (_,_) if(j%k == 0) => false
    case (_,_) if(j%k != 0) => isPrime(j,k-1)
  }

def listPrimesInRange(j: Int,k: Int): List[Any] = {

  (j,k) match{
    case (_,_) if(j>k) => return Nil;
    case (_,_) if( isPrime(j,j/2) ) => List(j):::listPrimesInRange(j+1,k);
    case (_,_) => listPrimesInRange(j+1,k);
  }

}
assert(listPrimesInRange(4,15) == List(5,7,11,13))
assert(listPrimesInRange(1,30) == List(2,3,5,7,11,13,17,19,23,29))


//9
def multipleOfThreeOrFive(n:Int): Int={
  if(n==0)
    return 0
  if(n%3==0||n%5==0)
    return n+multipleOfThreeOrFive(n-1)

  return multipleOfThreeOrFive(n-1)
}

assert(multipleOfThreeOrFive( 10 ) == 33 )
assert(multipleOfThreeOrFive ( 20 ) == 98 )
assert(multipleOfThreeOrFive ( 0 ) == 0 )

//10
def fullWords(number: Int): List[String] = {
  if(number<10) {
    if (number == 0)
      return List("zero")
    else if (number == 1)
      return List("one")
    else if (number == 2)
      return List("two")
    else if (number == 3)
      return List("three")
    else if (number == 4)
      return List("four")
    else if (number == 5)
      return List("five")
    else if (number == 6)
      return List("six")
    else if (number == 7)
      return List("seven")
    else if (number == 8)
      return List("eight")
    else  return List("nine")
  }

  else{
    if(number %10 == 0)
      return fullWords(number/10) ::: List("zero")
    else if ( number %10 == 1)
      return fullWords(number/10) ::: List("one")
    else if( number %10 == 2)
      return fullWords(number/10) ::: List("two")
    else if( number %10 == 3)
      return fullWords(number/10) ::: List("three")
    else if( number %10 == 4 )
      return fullWords(number/10) ::: List("four")
    else if(number %10 == 5)
      return fullWords(number/10) ::: List("five")
    else if(number %10 == 6)
      return fullWords(number/10) ::: List("six")
    else if(number %10 == 7)
      return fullWords(number/10) ::: List("seven")
    else if(number %10 == 8)
      return fullWords(number/10) ::: List("eight")
    else return fullWords(number/10) ::: List("nine")

  }
}

assert( fullWords ( 1000 ) == List( "one","zero","zero","zero" ) )
assert( fullWords ( 1234 ) == List( "one","two","three","four" ) )


//11
def noOfPaths ( l: Int, b: Int): Int = {

  if ( l ==1)
    return b +1

  else if ( b ==1 )
    return l +1

  else
    return ( noOfPaths (l-1,b ) + noOfPaths (l ,b-1 ) )
}

assert( noOfPaths ( 10, 10 ) == 184756)