(c-declare "#include \"gl/glu.h\"")
(include "types.scm")
(define GLU_EXT_object_space_tess 1)
(define GLU_EXT_nurbs_tessellator 1)
(define GLU_FALSE 0)
(define GLU_TRUE 1)
(define GLU_VERSION_1_1 1)
(define GLU_VERSION_1_2 1)
(define GLU_VERSION_1_3 1)
(define GLU_VERSION 100800)
(define GLU_EXTENSIONS 100801)
(define GLU_INVALID_ENUM 100900)
(define GLU_INVALID_VALUE 100901)
(define GLU_OUT_OF_MEMORY 100902)
(define GLU_INCOMPATIBLE_GL_VERSION 100903)
(define GLU_INVALID_OPERATION 100904)
(define GLU_OUTLINE_POLYGON 100240)
(define GLU_OUTLINE_PATCH 100241)
(define GLU_NURBS_ERROR 100103)
(define GLU_ERROR 100103)
(define GLU_NURBS_BEGIN 100164)
(define GLU_NURBS_BEGIN_EXT 100164)
(define GLU_NURBS_VERTEX 100165)
(define GLU_NURBS_VERTEX_EXT 100165)
(define GLU_NURBS_NORMAL 100166)
(define GLU_NURBS_NORMAL_EXT 100166)
(define GLU_NURBS_COLOR 100167)
(define GLU_NURBS_COLOR_EXT 100167)
(define GLU_NURBS_TEXTURE_COORD 100168)
(define GLU_NURBS_TEX_COORD_EXT 100168)
(define GLU_NURBS_END 100169)
(define GLU_NURBS_END_EXT 100169)
(define GLU_NURBS_BEGIN_DATA 100170)
(define GLU_NURBS_BEGIN_DATA_EXT 100170)
(define GLU_NURBS_VERTEX_DATA 100171)
(define GLU_NURBS_VERTEX_DATA_EXT 100171)
(define GLU_NURBS_NORMAL_DATA 100172)
(define GLU_NURBS_NORMAL_DATA_EXT 100172)
(define GLU_NURBS_COLOR_DATA 100173)
(define GLU_NURBS_COLOR_DATA_EXT 100173)
(define GLU_NURBS_TEXTURE_COORD_DATA 100174)
(define GLU_NURBS_TEX_COORD_DATA_EXT 100174)
(define GLU_NURBS_END_DATA 100175)
(define GLU_NURBS_END_DATA_EXT 100175)
(define GLU_NURBS_ERROR1 100251)
(define GLU_NURBS_ERROR2 100252)
(define GLU_NURBS_ERROR3 100253)
(define GLU_NURBS_ERROR4 100254)
(define GLU_NURBS_ERROR5 100255)
(define GLU_NURBS_ERROR6 100256)
(define GLU_NURBS_ERROR7 100257)
(define GLU_NURBS_ERROR8 100258)
(define GLU_NURBS_ERROR9 100259)
(define GLU_NURBS_ERROR10 100260)
(define GLU_NURBS_ERROR11 100261)
(define GLU_NURBS_ERROR12 100262)
(define GLU_NURBS_ERROR13 100263)
(define GLU_NURBS_ERROR14 100264)
(define GLU_NURBS_ERROR15 100265)
(define GLU_NURBS_ERROR16 100266)
(define GLU_NURBS_ERROR17 100267)
(define GLU_NURBS_ERROR18 100268)
(define GLU_NURBS_ERROR19 100269)
(define GLU_NURBS_ERROR20 100270)
(define GLU_NURBS_ERROR21 100271)
(define GLU_NURBS_ERROR22 100272)
(define GLU_NURBS_ERROR23 100273)
(define GLU_NURBS_ERROR24 100274)
(define GLU_NURBS_ERROR25 100275)
(define GLU_NURBS_ERROR26 100276)
(define GLU_NURBS_ERROR27 100277)
(define GLU_NURBS_ERROR28 100278)
(define GLU_NURBS_ERROR29 100279)
(define GLU_NURBS_ERROR30 100280)
(define GLU_NURBS_ERROR31 100281)
(define GLU_NURBS_ERROR32 100282)
(define GLU_NURBS_ERROR33 100283)
(define GLU_NURBS_ERROR34 100284)
(define GLU_NURBS_ERROR35 100285)
(define GLU_NURBS_ERROR36 100286)
(define GLU_NURBS_ERROR37 100287)
(define GLU_AUTO_LOAD_MATRIX 100200)
(define GLU_CULLING 100201)
(define GLU_SAMPLING_TOLERANCE 100203)
(define GLU_DISPLAY_MODE 100204)
(define GLU_PARAMETRIC_TOLERANCE 100202)
(define GLU_SAMPLING_METHOD 100205)
(define GLU_U_STEP 100206)
(define GLU_V_STEP 100207)
(define GLU_NURBS_MODE 100160)
(define GLU_NURBS_MODE_EXT 100160)
(define GLU_NURBS_TESSELLATOR 100161)
(define GLU_NURBS_TESSELLATOR_EXT 100161)
(define GLU_NURBS_RENDERER 100162)
(define GLU_NURBS_RENDERER_EXT 100162)
(define GLU_OBJECT_PARAMETRIC_ERROR 100208)
(define GLU_OBJECT_PARAMETRIC_ERROR_EXT 100208)
(define GLU_OBJECT_PATH_LENGTH 100209)
(define GLU_OBJECT_PATH_LENGTH_EXT 100209)
(define GLU_PATH_LENGTH 100215)
(define GLU_PARAMETRIC_ERROR 100216)
(define GLU_DOMAIN_DISTANCE 100217)
(define GLU_MAP1_TRIM_2 100210)
(define GLU_MAP1_TRIM_3 100211)
(define GLU_POINT 100010)
(define GLU_LINE 100011)
(define GLU_FILL 100012)
(define GLU_SILHOUETTE 100013)
(define GLU_SMOOTH 100000)
(define GLU_FLAT 100001)
(define GLU_NONE 100002)
(define GLU_OUTSIDE 100020)
(define GLU_INSIDE 100021)
(define GLU_TESS_BEGIN 100100)
(define GLU_BEGIN 100100)
(define GLU_TESS_VERTEX 100101)
(define GLU_VERTEX 100101)
(define GLU_TESS_END 100102)
(define GLU_END 100102)
(define GLU_TESS_ERROR 100103)
(define GLU_TESS_EDGE_FLAG 100104)
(define GLU_EDGE_FLAG 100104)
(define GLU_TESS_COMBINE 100105)
(define GLU_TESS_BEGIN_DATA 100106)
(define GLU_TESS_VERTEX_DATA 100107)
(define GLU_TESS_END_DATA 100108)
(define GLU_TESS_ERROR_DATA 100109)
(define GLU_TESS_EDGE_FLAG_DATA 100110)
(define GLU_TESS_COMBINE_DATA 100111)
(define GLU_CW 100120)
(define GLU_CCW 100121)
(define GLU_INTERIOR 100122)
(define GLU_EXTERIOR 100123)
(define GLU_UNKNOWN 100124)
(define GLU_TESS_WINDING_RULE 100140)
(define GLU_TESS_BOUNDARY_ONLY 100141)
(define GLU_TESS_TOLERANCE 100142)
(define GLU_TESS_ERROR1 100151)
(define GLU_TESS_ERROR2 100152)
(define GLU_TESS_ERROR3 100153)
(define GLU_TESS_ERROR4 100154)
(define GLU_TESS_ERROR5 100155)
(define GLU_TESS_ERROR6 100156)
(define GLU_TESS_ERROR7 100157)
(define GLU_TESS_ERROR8 100158)
(define GLU_TESS_MISSING_BEGIN_POLYGON 100151)
(define GLU_TESS_MISSING_BEGIN_CONTOUR 100152)
(define GLU_TESS_MISSING_END_POLYGON 100153)
(define GLU_TESS_MISSING_END_CONTOUR 100154)
(define GLU_TESS_COORD_TOO_LARGE 100155)
(define GLU_TESS_NEED_COMBINE_CALLBACK 100156)
(define GLU_TESS_WINDING_ODD 100130)
(define GLU_TESS_WINDING_NONZERO 100131)
(define GLU_TESS_WINDING_POSITIVE 100132)
(define GLU_TESS_WINDING_NEGATIVE 100133)
(define GLU_TESS_WINDING_ABS_GEQ_TWO 100134)
(c-define-type GLUnurbs (struct "GLUnurbs"))
(c-define-type GLUquadric (struct "GLUquadric"))
(c-define-type GLUtesselator (struct "GLUtesselator"))
(c-define-type GLUnurbsObj (struct "GLUnurbs"))
(c-define-type GLUquadricObj (struct "GLUquadric"))
(c-define-type GLUtesselatorObj (struct "GLUtesselator"))
(c-define-type GLUtriangulatorObj (struct "GLUtesselator"))
(define GLU_TESS_MAX_COORD 1e150)
(define gluBeginCurve (c-lambda ((pointer GLUnurbs)) void "gluBeginCurve"))
(define gluBeginPolygon (c-lambda ((pointer GLUtesselator)) void "gluBeginPolygon"))
(define gluBeginSurface (c-lambda ((pointer GLUnurbs)) void "gluBeginSurface"))
(define gluBeginTrim (c-lambda ((pointer GLUnurbs)) void "gluBeginTrim"))
(define gluBuild1DMipmapLevels (c-lambda (GLenum GLint GLsizei GLenum GLenum GLint GLint GLint (pointer void)) GLint "gluBuild1DMipmapLevels"))
(define gluBuild1DMipmaps (c-lambda (GLenum GLint GLsizei GLenum GLenum (pointer void)) GLint "gluBuild1DMipmaps"))
(define gluBuild2DMipmapLevels (c-lambda (GLenum GLint GLsizei GLsizei GLenum GLenum GLint GLint GLint (pointer void)) GLint "gluBuild2DMipmapLevels"))
(define gluBuild2DMipmaps (c-lambda (GLenum GLint GLsizei GLsizei GLenum GLenum (pointer void)) GLint "gluBuild2DMipmaps"))
(define gluBuild3DMipmapLevels (c-lambda (GLenum GLint GLsizei GLsizei GLsizei GLenum GLenum GLint GLint GLint (pointer void)) GLint "gluBuild3DMipmapLevels"))
(define gluBuild3DMipmaps (c-lambda (GLenum GLint GLsizei GLsizei GLsizei GLenum GLenum (pointer void)) GLint "gluBuild3DMipmaps"))
(define gluCheckExtension (c-lambda ((pointer GLubyte) (pointer GLubyte)) GLboolean "gluCheckExtension"))
(define gluCylinder (c-lambda ((pointer GLUquadric) GLdouble GLdouble GLdouble GLint GLint) void "gluCylinder"))
(define gluDeleteNurbsRenderer (c-lambda ((pointer GLUnurbs)) void "gluDeleteNurbsRenderer"))
(define gluDeleteQuadric (c-lambda ((pointer GLUquadric)) void "gluDeleteQuadric"))
(define gluDeleteTess (c-lambda ((pointer GLUtesselator)) void "gluDeleteTess"))
(define gluDisk (c-lambda ((pointer GLUquadric) GLdouble GLdouble GLint GLint) void "gluDisk"))
(define gluEndCurve (c-lambda ((pointer GLUnurbs)) void "gluEndCurve"))
(define gluEndPolygon (c-lambda ((pointer GLUtesselator)) void "gluEndPolygon"))
(define gluEndSurface (c-lambda ((pointer GLUnurbs)) void "gluEndSurface"))
(define gluEndTrim (c-lambda ((pointer GLUnurbs)) void "gluEndTrim"))
(define gluErrorString (c-lambda (GLenum) (pointer GLubyte) "gluErrorString"))
(define gluGetNurbsProperty (c-lambda ((pointer GLUnurbs) GLenum (pointer GLfloat)) void "gluGetNurbsProperty"))
(define gluGetString (c-lambda (GLenum) (pointer GLubyte) "gluGetString"))
(define gluGetTessProperty (c-lambda ((pointer GLUtesselator) GLenum (pointer GLdouble)) void "gluGetTessProperty"))
(define gluLoadSamplingMatrices (c-lambda ((pointer GLUnurbs) (pointer GLfloat) (pointer GLfloat) (pointer GLint)) void "gluLoadSamplingMatrices"))
(define gluLookAt (c-lambda (GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble) void "gluLookAt"))
(define gluNewNurbsRenderer (c-lambda () (pointer GLUnurbs) "gluNewNurbsRenderer"))
(define gluNewQuadric (c-lambda () (pointer GLUquadric) "gluNewQuadric"))
(define gluNewTess (c-lambda () (pointer GLUtesselator) "gluNewTess"))
(define gluNextContour (c-lambda ((pointer GLUtesselator) GLenum) void "gluNextContour"))
#!void
(define gluNurbsCallbackData (c-lambda ((pointer GLUnurbs) (pointer GLvoid)) void "gluNurbsCallbackData"))
(define gluNurbsCallbackDataEXT (c-lambda ((pointer GLUnurbs) (pointer GLvoid)) void "gluNurbsCallbackDataEXT"))
(define gluNurbsCurve (c-lambda ((pointer GLUnurbs) GLint (pointer GLfloat) GLint (pointer GLfloat) GLint GLenum) void "gluNurbsCurve"))
(define gluNurbsProperty (c-lambda ((pointer GLUnurbs) GLenum GLfloat) void "gluNurbsProperty"))
(define gluNurbsSurface (c-lambda ((pointer GLUnurbs) GLint (pointer GLfloat) GLint (pointer GLfloat) GLint GLint (pointer GLfloat) GLint GLint GLenum) void "gluNurbsSurface"))
(define gluOrtho2D (c-lambda (GLdouble GLdouble GLdouble GLdouble) void "gluOrtho2D"))
(define gluPartialDisk (c-lambda ((pointer GLUquadric) GLdouble GLdouble GLint GLint GLdouble GLdouble) void "gluPartialDisk"))
(define gluPerspective (c-lambda (GLdouble GLdouble GLdouble GLdouble) void "gluPerspective"))
(define gluPickMatrix (c-lambda (GLdouble GLdouble GLdouble GLdouble (pointer GLint)) void "gluPickMatrix"))
(define gluProject (c-lambda (GLdouble GLdouble GLdouble (pointer GLdouble) (pointer GLdouble) (pointer GLint) (pointer GLdouble) (pointer GLdouble) (pointer GLdouble)) GLint "gluProject"))
(define gluPwlCurve (c-lambda ((pointer GLUnurbs) GLint (pointer GLfloat) GLint GLenum) void "gluPwlCurve"))
#!void
(define gluQuadricDrawStyle (c-lambda ((pointer GLUquadric) GLenum) void "gluQuadricDrawStyle"))
(define gluQuadricNormals (c-lambda ((pointer GLUquadric) GLenum) void "gluQuadricNormals"))
(define gluQuadricOrientation (c-lambda ((pointer GLUquadric) GLenum) void "gluQuadricOrientation"))
(define gluQuadricTexture (c-lambda ((pointer GLUquadric) GLboolean) void "gluQuadricTexture"))
(define gluScaleImage (c-lambda (GLenum GLsizei GLsizei GLenum (pointer void) GLsizei GLsizei GLenum (pointer GLvoid)) GLint "gluScaleImage"))
(define gluSphere (c-lambda ((pointer GLUquadric) GLdouble GLint GLint) void "gluSphere"))
(define gluTessBeginContour (c-lambda ((pointer GLUtesselator)) void "gluTessBeginContour"))
(define gluTessBeginPolygon (c-lambda ((pointer GLUtesselator) (pointer GLvoid)) void "gluTessBeginPolygon"))
#!void
(define gluTessEndContour (c-lambda ((pointer GLUtesselator)) void "gluTessEndContour"))
(define gluTessEndPolygon (c-lambda ((pointer GLUtesselator)) void "gluTessEndPolygon"))
(define gluTessNormal (c-lambda ((pointer GLUtesselator) GLdouble GLdouble GLdouble) void "gluTessNormal"))
(define gluTessProperty (c-lambda ((pointer GLUtesselator) GLenum GLdouble) void "gluTessProperty"))
(define gluTessVertex (c-lambda ((pointer GLUtesselator) (pointer GLdouble) (pointer GLvoid)) void "gluTessVertex"))
(define gluUnProject (c-lambda (GLdouble GLdouble GLdouble (pointer GLdouble) (pointer GLdouble) (pointer GLint) (pointer GLdouble) (pointer GLdouble) (pointer GLdouble)) GLint "gluUnProject"))
(define gluUnProject4 (c-lambda (GLdouble GLdouble GLdouble GLdouble (pointer GLdouble) (pointer GLdouble) (pointer GLint) GLdouble GLdouble (pointer GLdouble) (pointer GLdouble) (pointer GLdouble) (pointer GLdouble)) GLint "gluUnProject4"))