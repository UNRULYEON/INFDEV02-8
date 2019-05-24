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
/// Exercise 5: Implement a function that returns a string containing all even numbers between lower and upper. Separate the numbers with a white space.
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
let rec allEvenRange =
  fun (l: int) -> fun (h: int) ->
    if (l >= h) then ""
    else (if (l % 2 = 0) then string l else "") + " " + allEvenRange(l + 1)(h)

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

/// **Description**
///
/// Exercise 8: Implement a function that returns a string containing the binary representation of the input number (it must be positive).
///
/// **Parameters**
///   * `n` - parameter of type `int`
///
/// **Output Type**
///   * `string`
///
/// **Exceptions**
///
let rec toBinary =
  fun (n) ->
    if (n <= 0) then ""
    else string (toBinary(n / 2) + string (n % 2))


/// **Description**
///
/// Exercise 9: Implement a function that returns a string containing the representation of the input number in an arbitrary base (the number must be positive). The algorithm is the same as above except you must take the remainder of n divided by base.
///
/// **Parameters**
///   * `n` - parameter of type `int`
///   * `b` - parameter of type `int`
///
/// **Output Type**
///   * `string`
///
/// **Exceptions**
///
let rec toBase =
  fun(n: int) -> fun (b: int) ->
    if (n <= 0) then ""
    else string (toBase(n / b)(b) + string (n % 2))