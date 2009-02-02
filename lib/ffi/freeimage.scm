
(c-declare "#include \"FreeImage.h\"")
(c-declare "#include \"stdlib.h\"")

(c-define-type FIBITMAP "FIBITMAP")
(c-define-type FIBITMAP* (pointer FIBITMAP))

(define freeimage-initialize
 (c-lambda (bool) void "FreeImage_Initialise"))

(define freeimage-deinitialize
 (c-lambda () void "FreeImage_DeInitialise"))

(define freeimage-load-jpg
  (c-lambda (char-string) FIBITMAP* #<<end-c-code
   FIBITMAP *img = FreeImage_Load(FIF_JPEG, ___arg1, JPEG_ACCURATE);
   ___result_voidstar = img;
end-c-code
))

(define freeimage-width
  (c-lambda (FIBITMAP*) int "FreeImage_GetWidth"))

(define freeimage-height
  (c-lambda (FIBITMAP*) int "FreeImage_GetHeight"))

(define freeimage-bytes
  (c-lambda (FIBITMAP*) (pointer unsigned-char) #<<end-c-code
   FIBITMAP* img = ___arg1;
   int width = FreeImage_GetWidth(img);
   int height = FreeImage_GetHeight(img);
   unsigned char *data = malloc(width*height*3);

   int i=0;
   RGBQUAD pixel;
   while(i<width*height) {
	   FreeImage_GetPixelColor(img, i%width, i/width, &pixel);
	   data[i*3] = pixel.rgbRed;
	   data[i*3+1] = pixel.rgbGreen;
	   data[i*3+2] = pixel.rgbBlue;
	   i++;
   }

   ___result_voidstar = data;
end-c-code
))
