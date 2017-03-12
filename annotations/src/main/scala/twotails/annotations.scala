package twotails

import scala.annotation.{StaticAnnotation, compileTimeOnly}

@compileTimeOnly("Unable to mutually tail call optimize this.")
final class mutualrec extends StaticAnnotation