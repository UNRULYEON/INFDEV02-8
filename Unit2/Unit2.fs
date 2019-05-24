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

let testlist = [1; 2; 3]
let testl1 = [1; 2; 3; 4; 5]
let testl2 = [6; 7; 8; 9; 10]