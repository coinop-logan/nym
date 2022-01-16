module Scene3dHelpers exposing (..)

import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Scene3d
import Vector3 exposing (Vector3)


mirrorGroup =
    Scene3d.mirrorAcross Plane3d.yz
