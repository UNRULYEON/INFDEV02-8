/// **Description**
///
/// Exercise 1: Implement a function that returns a string containing all numbers from 0 to n. Separate the numbers with a white space.
///
/// **Parameters**
///   * `int`
///
/// **Output Type**
///   * `string`
///
let rec allNumber (n: int) =
  if n <= 0 then
    string n
  else
    allNumber(n - 1) + " " + string n

// let rec allNumber =
//   fun (n: int) ->
//     if n <= 0 then
//       string n
//     else
//       allNumber(n - 1) + " " + string n


/// **Description**
///
/// Exercise 2: Implement a function that returns a string containing all numbers from n to 0. Separate the numbers with a white space.
///
/// **Parameters**
///   * `n` - parameter of type `int`
///
/// **Output Type**
///   * `string`
///
/// **Exceptions**
///
let rec allNumberRev (n: int) =
  if n <= 0 then
    string n
  else
    string n + " " + allNumberRev(n - 1)

// let rec allNumberRev =
//   fun (n: int) ->
//     if n <= 0 then
//       string n
//     else
//       string n + " " + allNumberRev(n - 1)


/// **Description**
///
/// Exercise 3: Implement a function that returns a string containing all numbers between lower and upper. Separate the numbers with a white space.
///
/// **Parameters**
///   * `l` - parameter of type `int`
///   * `h` - parameter of type `int`
///
/// **Output Type**
///   * `string`
///
/// **Exceptions**
///
let rec allNumberRange =
  fun l -> fun h ->
    if (l >= h) then string l
    else string l + " " + allNumberRange(l + 1)(h)


/// **Description**
///
/// Exercise 4: Implement a function that returns a string containing all numbers between lower and upper in reverse order. Sepa- rate the numbers with a white space.
///
/// **Parameters**
///   * `l` - parameter of type `int`
///   * `h` - parameter of type `int`
///
/// **Output Type**
///   * `string`
///
/// **Exceptions**
///
let rec allNumberRangeRev =
  fun l -> fun h ->
   if (h <= l) then string h
   else string h + " " + allNumberRangeRev(l)(h - 1)

/// **Description**
///
/// Exercise 6: Implement a function that returns a string containing length asterisks.
///
/// **Parameters**
///   * `n` - parameter of type `int`
///
/// **Output Type**
///   * `string`
///
/// **Exceptions**
///
let rec drawLine =
  fun (n: int) ->
    if (n <= 0) then
      ""
    else
      "*" + drawLine(n - 1)

/// **Description**
///
/// Exercise 7: Implement a function that returns a string containing length repetitions of symbol.
///
/// **Parameters**
///   * `s` - parameter of type `string`
///   * `n` - parameter of type `int`
///
/// **Output Type**
///   * `string`
///
/// **Exceptions**
///
let rec drawSymbols =
  fun (s: string) -> fun (n: int) ->
    if (n <= 0) then ""
    else s + drawSymbols (s) (n - 1)