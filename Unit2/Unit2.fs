
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
/// List to test the last function
///
/// **Parameters**
///
///
/// **Output Type**
///   * `int list`
///
/// **Exceptions**
///
let lastTest = [1; 2; 3]