open Graphics

(* Takes in a string for the file name of a pbm file, assumes that it is correct
   as this function should only be called by "view.ml".
   Returns : a color array array that represents the image file that can be
    displayed by the graphics module *)
val rip : string -> color array array

(* Does the same as rip, but instead overlays the color parameter over the image
   before it is returned, for things such as colorizing a grayscale image *)
val ripColorize : string -> color -> color array array