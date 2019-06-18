open System
open System.Windows.Forms
let r = Random()

type Point2D = {
  Position : (float * float)
} with
  static member Create (x : float, y : float) : Point2D = {
    Position = (x, y)
  }
  static member CreateRandom (min : int, max : int) : Point2D =
    let x = r.Next(min, max) |> float
    let y = r.Next(min, max) |> float
    Point2D.Create(x, y)
  member this.X = fst this.Position
  member this.Y = snd this.Position
  member this.CalcDistance (point : Point2D) =
    Math.Sqrt((this.X - point.X) * (this.X - point.X) + (this.Y - point.Y) * (this.Y - point.Y))

let clamp min max value =
  if value < min then
    min
  elif value > max then
    max
  else
    value

type Blob = {
  Position : Point2D
  Size : int
} with
  static member Create () = {
    Position = Point2D.CreateRandom(-50, 50)
    Size = r.Next(1, 6)
  }
  member this.Speed = (float this.Size) / 10.0
  member this.Up () = {this with Position = Point2D.Create(this.Position.X, clamp -50.0 50.0 (this.Position.Y + this.Speed))}
  member this.Right () = {this with Position = Point2D.Create(clamp -50.0 50.0 (this.Position.X - this.Speed), this.Position.Y)}
  member this.Down () = {this with Position = Point2D.Create(this.Position.X, clamp -50.0 50.0 (this.Position.Y + this.Speed))}
  member this.Left () = {this with Position = Point2D.Create(clamp -50.0 50.0 (this.Position.X + this.Speed), this.Position.Y)}
  member this.Move () =
    let decision = r.Next(0, 4)
    printfn "Decision: %i" decision
    match decision with
      | 0 -> this.Up()
      | 1 -> this.Right()
      | 2 -> this.Down()
      | 3 -> this.Left()
      | _ -> this

type World = {
  Blob1 : Blob
  Blob2 : Blob
  Tick : int
} with
  static member Create (t: int) = {
    Blob1 = Blob.Create()
    Blob2 = Blob.Create()
    Tick = t
  }
  member this.Run() =
    for i = 1 to this.Tick do
      printfn "Currently on tick: %i/%i" i this.Tick

      printfn "Moving Blob 1..."
      this.Blob1.Move() |> ignore

      printfn "Moving Blob 2..."
      this.Blob2.Move() |> ignore

      printfn "Location of Blob 1: %A" this.Blob1.Position.Position
      printfn "Location of Blob 2: %A" this.Blob2.Position.Position
      printfn ""



