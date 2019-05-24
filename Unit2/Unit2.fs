// type List <a> = {
//   kind : "empty"
// } | {
//   kind : "cons"
//   head : a
//   tail : List <a>
// }

/// **Description**
///
/// Exercise 1: Implement a function that returns the last element of a list
///
/// **Parameters**
///   * `l` - parameter of type `List<'a>`
///
/// **Output Type**
///   * `List<'a>`
///
/// **Exceptions**
///
let rec last =
  fun (l: List<'a>) ->
    if (l.Tail.IsEmpty) then
      l
    else
      last(l.Tail)


/// **Description**
///
/// Exercise 2: Implement a function that creates a list with the elements of l in reverse order.
///
/// **Parameters**
///   * `l` - parameter of type `List<'a>`
///
/// **Output Type**
///   * `List<'a>`
///
/// **Exceptions**
///
let rec rev (l: List<'a>) : List<'a> =
  if (l.IsEmpty) then
    []
  else
    (rev l.Tail) @ [l.Head] // With @ you can concatenate two lists -> https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists#operators-for-working-with-lists


/// **Description**
///
/// Exercise 3: Implement a function that adds all the elements of l2 after those in l1.
///
/// **Parameters**
///   * `l1` - parameter of type `List<'a>`
///   * `l2` - parameter of type `List<'a>`
///
/// **Output Type**
///   * `List<'a>`
///
/// **Exceptions**
///
let append (l1: List<'a>) (l2: List<'a>) : List<'a> =
  l1 @ l2 // With @ you can concatenate two lists -> https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists#operators-for-working-with-lists


/// **Description**
///
/// Exercise 4: Implement a function that returns the element in position n in l.
///
/// **Parameters**
///   * `n` - parameter of type `int`
///   * `l` - parameter of type `List<'a>`
///
/// **Output Type**
///   * `Option<'a>`
///
/// **Exceptions**
///
let rec nth (n: int) (l: List<'a>) : Option<'a> =
  if (l.IsEmpty) then
    None  // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/options#using-options
  elif (n = 0) then
    Some l.Head
  else
    nth (n - 1) l.Tail


/// **Description**
///
/// Exercise 5: Implement a function that checks if a list is palindrome. A list is palindrome if it is equal to its inverse.
///
/// **Parameters**
///   * `l` - parameter of type `List<'a>`
///
/// **Output Type**
///   * `bool`
///
/// **Exceptions**
///
let rec palindrome (l: List<'a>) : bool =
  l = (rev l)


/// **Description**
///
/// Exercise 6: Implement a function that removes consecutive occurrences of the same element in the list. For example compress [a;a;a;a;b;b;c;c;b] = [a;b;c;b].
///
/// **Parameters**
///   * `l` - parameter of type `List<'a>`
///
/// **Output Type**
///   * `List<'a>`
///
/// **Exceptions**
///
let rec compress (l: List<'a>) : List<'a> =
  if l.IsEmpty then []
  elif l.Tail.IsEmpty then [l.Head]
  else
    let x = l.Head
    let y = l.Tail.Head
    let xs = l.Tail.Tail
    if x = y then
      compress (y :: xs)
    else
      x :: (compress (y :: xs))

let testlist = [1; 2; 3]
let testlist2 = [1; 2; 3; 2; 1]
let testlist3 = [1; 2; 2; 3; 3; 3; 4; 4; 4; 4;]
let testl1 = [1; 2; 3; 4; 5]
let testl2 = [6; 7; 8; 9; 10]