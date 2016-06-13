package twotails

import scala.annotation.{StaticAnnotation, compileTimeOnly}

@compileTimeOnly("Unable to tail call optimize as this is either not effectively final or a non-constructor method.")
final class mutualrec extends StaticAnnotation