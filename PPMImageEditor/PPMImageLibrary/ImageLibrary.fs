﻿module PPMImageLibrary

#light


//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage (image:(int*int*int) list list) = 
  match image with
  | [] -> printfn "**END**"
  | hd::tl -> printfn "%A" hd
              OutputImage tl
           
let DebugOutput(width:int, height:int, depth:int, image:(int*int*int) list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage image


//
// TransformFirstThreeRows:
//
// An example transformation: replaces the first 3 rows of the given image
// with a row of Red, White and Blue pixels (go USA :-).
//
let rec BuildRowOfThisColor row color = 
  match row with
  | []     -> []
  | hd::tl -> color :: BuildRowOfThisColor tl color

let TransformFirstThreeRows(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let row1 = List.head image
  let row2 = List.head (List.tail image)
  let row3 = List.head (List.tail (List.tail image))
  let tail = List.tail (List.tail (List.tail image))
  let newRow1 = BuildRowOfThisColor row1 (255,0,0)      // red:
  let newRow2 = BuildRowOfThisColor row2 (255,255,255)  // white:
  let newRow3 = BuildRowOfThisColor row3 (0,0,255)      // blue:
  let newImage = newRow1 :: newRow2 :: newRow3 :: tail
  newImage


//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let Flatten (SL:string list) = 
  List.reduce (fun s1 s2 -> s1 + " " + s2) SL

let Image2ListOfStrings (image:(int*int*int) list list) = 
  List.map (fun TL -> List.map (fun (r,g,b) -> r.ToString()+" "+g.ToString()+" "+b.ToString()+" ") TL) image
  |> List.map Flatten

let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = [ "P3" ] @ 
          [ System.Convert.ToString(width); System.Convert.ToString(height) ] @
          [ System.Convert.ToString(depth) ] @
          (Image2ListOfStrings image)
  System.IO.File.WriteAllLines(filepath, L)
  true  // success



//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//

let average tuple = 
    let (a,b,c) = tuple
    let av = (a+b+c)/3
    (av,av,av)

let helpRow L = List.map average L
let helpImg img = List.map helpRow img

let rec Grayscale(width:int, height:int, depth:int, image:(int*int*int) list list) = helpImg image




//
// Threshold
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//

let thres tuple th =
    let (a,b,c) = tuple
    let na = if a <= th then 0 else 255
    let nb = if b <= th then 0 else 255
    let nc = if c <= th then 0 else 255
    (na,nb,nc)

let threRow L = List.map thres L
let threImg L = List.map threRow L
let rec Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = threImg image



//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row.
//

let flipRow L = 
    let mid = (List.length L)/2
    let (L1,L2) = List.splitAt mid L
    let revL1 = List.rev L1
    let revL2 = List.rev L2
    revL2@revL1


let rec FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = List.map flipRow image



//
// Zoom:
//
// Zooms the image by the given zoom factor, which is an integer 0 < factor < 5.  
// The function uses the nearest neighbor approach where each pixel P in the original 
// image is replaced by a factor*factor block of P pixels.  For example, if the zoom 
// factor is 4, then each pixel is replaced by a 4x4 block of 16 identical pixels. 
// The nearest neighbor algorithm is the simplest zoom algorithm, but results in 
// jagged images.  The resulting image is returned.
//

let rec magPix pix fac newSetPix = 
  match fac with
  | -1 -> newSetPix
  | _ -> magPix pix (fac-1) (pix::newSetPix)

let rec magRowHelp row fac newRow = 
  match row with
  | [] -> newRow
  | hl::tl -> magRowHelp tl fac newRow@(magPix hl fac [])

let rec magRowHelp2 fac newRow newSetRow = 
  match fac with
  | -1 -> newSetRow
  | _ -> magRowHelp2 (fac-1) newRow (newRow::newSetRow)
  

let rec mag img fac newImg  = 
  match img with
  | [] -> newImg
  | hl::tl -> mag tl fac newImg@(magRowHelp2 fac (List.rev (magRowHelp hl fac [])) [])

let rec Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
  image



//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//

let rec rotateRow L partImg newImg = 
    match L, partImg with
    | [], [] -> newImg
    | hl::tl , l1::lt -> rotateRow tl lt [(hl::l1)]@newImg
    | [], _ -> newImg
    | _, [] -> newImg
    

let helpRoRow L partImg newImg = List.rev (rotateRow L partImg newImg)

let rec rotateMatrix M Mc =
  match M with
  | [] -> List.rev Mc
  | hl::tl -> (rotateMatrix tl (helpRoRow hl Mc []))

let rotateMhelper M = List.rev (rotateMatrix M (List.map (fun x -> [])  (List.item 0 M) ))

let rec RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  image
