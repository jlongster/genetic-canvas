#ifdef ___LINKER_INFO
; File: "init.c", produced by Gambit-C v4.3.2
(
403002
" init"
(" init")
(
)
(
)
(
" init"
"join-paths"
"path-trim"
"resource"
)
(
"init-engine-c"
"init-opengl-c"
"lib-path"
"resource-path"
"run-frame-c"
"shutdown-engine-c"
)
(
"-"
"apply"
"car"
"cdr"
"cons"
"eq?"
"init-engine"
"init-opengl"
"load"
"null?"
"run-frame"
"shutdown-engine"
"string-append"
"string-length"
"string-ref"
"substring"
)
 #f
)
#else
#define ___VERSION 403002
#define ___MODULE_NAME " init"
#define ___LINKER_ID ____20_init
#define ___MH_PROC ___H__20_init
#define ___SCRIPT_LINE 0
#define ___GLO_COUNT 26
#define ___SUP_COUNT 10
#define ___SUB_COUNT 7
#define ___LBL_COUNT 88
#include "gambit.h"

___NEED_GLO(___G__20_init)
___NEED_GLO(___G__2d_)
___NEED_GLO(___G_apply)
___NEED_GLO(___G_car)
___NEED_GLO(___G_cdr)
___NEED_GLO(___G_cons)
___NEED_GLO(___G_eq_3f_)
___NEED_GLO(___G_init_2d_engine)
___NEED_GLO(___G_init_2d_engine_2d_c)
___NEED_GLO(___G_init_2d_opengl)
___NEED_GLO(___G_init_2d_opengl_2d_c)
___NEED_GLO(___G_join_2d_paths)
___NEED_GLO(___G_lib_2d_path)
___NEED_GLO(___G_load)
___NEED_GLO(___G_null_3f_)
___NEED_GLO(___G_path_2d_trim)
___NEED_GLO(___G_resource)
___NEED_GLO(___G_resource_2d_path)
___NEED_GLO(___G_run_2d_frame)
___NEED_GLO(___G_run_2d_frame_2d_c)
___NEED_GLO(___G_shutdown_2d_engine)
___NEED_GLO(___G_shutdown_2d_engine_2d_c)
___NEED_GLO(___G_string_2d_append)
___NEED_GLO(___G_string_2d_length)
___NEED_GLO(___G_string_2d_ref)
___NEED_GLO(___G_substring)

___BEGIN_GLO
___DEF_GLO(0," init")
___DEF_GLO(1,"init-engine-c")
___DEF_GLO(2,"init-opengl-c")
___DEF_GLO(3,"join-paths")
___DEF_GLO(4,"lib-path")
___DEF_GLO(5,"path-trim")
___DEF_GLO(6,"resource")
___DEF_GLO(7,"resource-path")
___DEF_GLO(8,"run-frame-c")
___DEF_GLO(9,"shutdown-engine-c")
___DEF_GLO(10,"-")
___DEF_GLO(11,"apply")
___DEF_GLO(12,"car")
___DEF_GLO(13,"cdr")
___DEF_GLO(14,"cons")
___DEF_GLO(15,"eq?")
___DEF_GLO(16,"init-engine")
___DEF_GLO(17,"init-opengl")
___DEF_GLO(18,"load")
___DEF_GLO(19,"null?")
___DEF_GLO(20,"run-frame")
___DEF_GLO(21,"shutdown-engine")
___DEF_GLO(22,"string-append")
___DEF_GLO(23,"string-length")
___DEF_GLO(24,"string-ref")
___DEF_GLO(25,"substring")
___END_GLO

___DEF_SUB_STR(___X0,3)
               ___STR3(108,105,98)
___DEF_SUB_STR(___X1,9)
               ___STR8(114,101,115,111,117,114,99,101)
               ___STR1(115)
___DEF_SUB_STR(___X2,10)
               ___STR8(108,105,98,47,101,110,103,105)
               ___STR2(110,101)
___DEF_SUB_STR(___X3,1)
               ___STR1(47)
___DEF_SUB_STR(___X4,1)
               ___STR1(47)
___DEF_SUB_STR(___X5,0)
               ___STR0
___DEF_SUB_STR(___X6,36)
               ___STR8(47,85,115,101,114,115,47,106)
               ___STR8(97,109,101,115,47,112,114,111)
               ___STR8(106,101,99,116,115,47,115,99)
               ___STR8(104,101,109,101,47,97,114,116)
               ___STR4(98,111,116,47)

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
___END_SUB


#define ___C_LBL_init_opengl 77
#define ___C_LBL_init_engine 80
#define ___C_LBL_shutdown_engine 83
#define ___C_LBL_run_frame 86

 void init_opengl ___PVOID
{
#define ___NARGS 0
___BEGIN_SFUN_VOID(___MLBL(___C_LBL_init_opengl))
___BEGIN_SFUN_BODY
___SFUN_CALL_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_BODY
___SFUN_ERROR_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_VOID
#undef ___NARGS
}

 void init_engine ___P((unsigned int ___arg1,unsigned int ___arg2),(___arg1,___arg2)
unsigned int ___arg1;
unsigned int ___arg2;)
{
#define ___NARGS 2
___BEGIN_SFUN_VOID(___MLBL(___C_LBL_init_engine))
___BEGIN_SFUN_ARG(1,___ARG1)
___BEGIN_SFUN_UINT_TO_SCMOBJ(___arg1,___ARG1,1)
___BEGIN_SFUN_ARG(2,___ARG2)
___BEGIN_SFUN_UINT_TO_SCMOBJ(___arg2,___ARG2,2)
___BEGIN_SFUN_BODY
___SFUN_ARG(1,___ARG1)
___SFUN_ARG(2,___ARG2)
___SFUN_CALL_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_BODY
___END_SFUN_UINT_TO_SCMOBJ(___arg2,___ARG2,2)
___END_SFUN_ARG(2)
___END_SFUN_UINT_TO_SCMOBJ(___arg1,___ARG1,1)
___END_SFUN_ARG(1)
___SFUN_ERROR_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_VOID
#undef ___NARGS
}

 void shutdown_engine ___PVOID
{
#define ___NARGS 0
___BEGIN_SFUN_VOID(___MLBL(___C_LBL_shutdown_engine))
___BEGIN_SFUN_BODY
___SFUN_CALL_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_BODY
___SFUN_ERROR_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_VOID
#undef ___NARGS
}

 void run_frame ___PVOID
{
#define ___NARGS 0
___BEGIN_SFUN_VOID(___MLBL(___C_LBL_run_frame))
___BEGIN_SFUN_BODY
___SFUN_CALL_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_BODY
___SFUN_ERROR_VOID
___SFUN_SET_RESULT_VOID
___END_SFUN_VOID
#undef ___NARGS
}


#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_init)
___DEF_M_HLBL(___L1__20_init)
___DEF_M_HLBL(___L2__20_init)
___DEF_M_HLBL(___L3__20_init)
___DEF_M_HLBL(___L4__20_init)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_path_2d_trim)
___DEF_M_HLBL(___L1_path_2d_trim)
___DEF_M_HLBL(___L2_path_2d_trim)
___DEF_M_HLBL(___L3_path_2d_trim)
___DEF_M_HLBL(___L4_path_2d_trim)
___DEF_M_HLBL(___L5_path_2d_trim)
___DEF_M_HLBL(___L6_path_2d_trim)
___DEF_M_HLBL(___L7_path_2d_trim)
___DEF_M_HLBL(___L8_path_2d_trim)
___DEF_M_HLBL(___L9_path_2d_trim)
___DEF_M_HLBL(___L10_path_2d_trim)
___DEF_M_HLBL(___L11_path_2d_trim)
___DEF_M_HLBL(___L12_path_2d_trim)
___DEF_M_HLBL(___L13_path_2d_trim)
___DEF_M_HLBL(___L14_path_2d_trim)
___DEF_M_HLBL(___L15_path_2d_trim)
___DEF_M_HLBL(___L16_path_2d_trim)
___DEF_M_HLBL(___L17_path_2d_trim)
___DEF_M_HLBL(___L18_path_2d_trim)
___DEF_M_HLBL(___L19_path_2d_trim)
___DEF_M_HLBL(___L20_path_2d_trim)
___DEF_M_HLBL(___L21_path_2d_trim)
___DEF_M_HLBL(___L22_path_2d_trim)
___DEF_M_HLBL(___L23_path_2d_trim)
___DEF_M_HLBL(___L24_path_2d_trim)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_join_2d_paths)
___DEF_M_HLBL(___L1_join_2d_paths)
___DEF_M_HLBL(___L2_join_2d_paths)
___DEF_M_HLBL(___L3_join_2d_paths)
___DEF_M_HLBL(___L4_join_2d_paths)
___DEF_M_HLBL(___L5_join_2d_paths)
___DEF_M_HLBL(___L6_join_2d_paths)
___DEF_M_HLBL(___L7_join_2d_paths)
___DEF_M_HLBL(___L8_join_2d_paths)
___DEF_M_HLBL(___L9_join_2d_paths)
___DEF_M_HLBL(___L10_join_2d_paths)
___DEF_M_HLBL(___L11_join_2d_paths)
___DEF_M_HLBL(___L12_join_2d_paths)
___DEF_M_HLBL(___L13_join_2d_paths)
___DEF_M_HLBL(___L14_join_2d_paths)
___DEF_M_HLBL(___L15_join_2d_paths)
___DEF_M_HLBL(___L16_join_2d_paths)
___DEF_M_HLBL(___L17_join_2d_paths)
___DEF_M_HLBL(___L18_join_2d_paths)
___DEF_M_HLBL(___L19_join_2d_paths)
___DEF_M_HLBL(___L20_join_2d_paths)
___DEF_M_HLBL(___L21_join_2d_paths)
___DEF_M_HLBL(___L22_join_2d_paths)
___DEF_M_HLBL(___L23_join_2d_paths)
___DEF_M_HLBL(___L24_join_2d_paths)
___DEF_M_HLBL(___L25_join_2d_paths)
___DEF_M_HLBL(___L26_join_2d_paths)
___DEF_M_HLBL(___L27_join_2d_paths)
___DEF_M_HLBL(___L28_join_2d_paths)
___DEF_M_HLBL(___L29_join_2d_paths)
___DEF_M_HLBL(___L30_join_2d_paths)
___DEF_M_HLBL(___L31_join_2d_paths)
___DEF_M_HLBL(___L32_join_2d_paths)
___DEF_M_HLBL(___L33_join_2d_paths)
___DEF_M_HLBL(___L34_join_2d_paths)
___DEF_M_HLBL(___L35_join_2d_paths)
___DEF_M_HLBL(___L36_join_2d_paths)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_resource)
___DEF_M_HLBL(___L1_resource)
___DEF_M_HLBL(___L2_resource)
___DEF_M_HLBL(___L3_resource)
___DEF_M_HLBL(___L4_resource)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_init_2d_opengl_2d_c)
___DEF_M_HLBL(___L1_init_2d_opengl_2d_c)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_init_2d_engine_2d_c)
___DEF_M_HLBL(___L1_init_2d_engine_2d_c)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_shutdown_2d_engine_2d_c)
___DEF_M_HLBL(___L1_shutdown_2d_engine_2d_c)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_run_2d_frame_2d_c)
___DEF_M_HLBL(___L1_run_2d_frame_2d_c)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_init
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_init)
___DEF_P_HLBL(___L1__20_init)
___DEF_P_HLBL(___L2__20_init)
___DEF_P_HLBL(___L3__20_init)
___DEF_P_HLBL(___L4__20_init)
___END_P_HLBL
___BEGIN_P_SW
#line 2 "resources.scm"
___DEF_SLBL(0,___L0__20_init)
#line 2
   ___IF_NARGS_EQ(0,___NOTHING)
#line 2
   ___WRONG_NARGS(0,0,0,0)
#line 2
___DEF_GLBL(___L__20_init)
#line 2
   ___SET_GLO(4,___G_lib_2d_path,___SUB(0))
   ___SET_GLO(7,___G_resource_2d_path,___SUB(1))
#line 5
   ___SET_GLO(5,___G_path_2d_trim,___PRC(7))
#line 19
   ___SET_GLO(3,___G_join_2d_paths,___PRC(33))
#line 34
   ___SET_GLO(6,___G_resource,___PRC(71))
#line 6 "init.scm"
   ___SET_STK(1,___R0)
#line 6
   ___SET_R1(___SUB(2))
#line 6
   ___SET_R0(___LBL(2))
#line 6
   ___ADJFP(4)
#line 6
   ___POLL(1)
#line 6
___DEF_SLBL(1,___L1__20_init)
#line 6
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G_resource)
#line 6
___DEF_SLBL(2,___L2__20_init)
#line 6
   ___SET_R0(___LBL(3))
#line 6
   ___JUMPGLOSAFE(___SET_NARGS(1),18,___G_load)
#line 6
___DEF_SLBL(3,___L3__20_init)
#line 9
   ___SET_GLO(2,___G_init_2d_opengl_2d_c,___PRC(77))
#line 12
   ___SET_GLO(1,___G_init_2d_engine_2d_c,___PRC(80))
#line 16
   ___SET_GLO(9,___G_shutdown_2d_engine_2d_c,___PRC(83))
#line 19
   ___SET_GLO(8,___G_run_2d_frame_2d_c,___PRC(86))
#line 19
   ___SET_R1(___VOID)
#line 19
   ___POLL(4)
#line 19
___DEF_SLBL(4,___L4__20_init)
#line 19
   ___ADJFP(-4)
#line 19
   ___JUMPPRM(___NOTHING,___STK(1))
#line 333 "init.c"
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_path_2d_trim
#undef ___PH_LBL0
#define ___PH_LBL0 7
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_path_2d_trim)
___DEF_P_HLBL(___L1_path_2d_trim)
___DEF_P_HLBL(___L2_path_2d_trim)
___DEF_P_HLBL(___L3_path_2d_trim)
___DEF_P_HLBL(___L4_path_2d_trim)
___DEF_P_HLBL(___L5_path_2d_trim)
___DEF_P_HLBL(___L6_path_2d_trim)
___DEF_P_HLBL(___L7_path_2d_trim)
___DEF_P_HLBL(___L8_path_2d_trim)
___DEF_P_HLBL(___L9_path_2d_trim)
___DEF_P_HLBL(___L10_path_2d_trim)
___DEF_P_HLBL(___L11_path_2d_trim)
___DEF_P_HLBL(___L12_path_2d_trim)
___DEF_P_HLBL(___L13_path_2d_trim)
___DEF_P_HLBL(___L14_path_2d_trim)
___DEF_P_HLBL(___L15_path_2d_trim)
___DEF_P_HLBL(___L16_path_2d_trim)
___DEF_P_HLBL(___L17_path_2d_trim)
___DEF_P_HLBL(___L18_path_2d_trim)
___DEF_P_HLBL(___L19_path_2d_trim)
___DEF_P_HLBL(___L20_path_2d_trim)
___DEF_P_HLBL(___L21_path_2d_trim)
___DEF_P_HLBL(___L22_path_2d_trim)
___DEF_P_HLBL(___L23_path_2d_trim)
___DEF_P_HLBL(___L24_path_2d_trim)
___END_P_HLBL
___BEGIN_P_SW
#line 5 "resources.scm"
___DEF_SLBL(0,___L0_path_2d_trim)
#line 5
   ___IF_NARGS_EQ(1,___NOTHING)
#line 5
   ___WRONG_NARGS(0,1,0,0)
#line 5
___DEF_GLBL(___L_path_2d_trim)
   ___SET_R1(___BOX(___R1))
   ___SET_R2(___UNBOX(___R1))
#line 7
   ___CHECK_HEAP(1,4096)
#line 7
___DEF_SLBL(1,___L1_path_2d_trim)
#line 7
   ___IF(___NOT(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref))))
#line 7
   ___GOTO(___L47_path_2d_trim)
#line 7
   ___END_IF
#line 7
   ___IF(___NOT(___STRINGP(___R2)))
#line 7
   ___GOTO(___L47_path_2d_trim)
#line 7
   ___END_IF
#line 7
   ___SET_R3(___STRINGLENGTH(___R2))
#line 7
   ___IF(___NOT(___FIXLT(___FIX(0L),___R3)))
#line 7
   ___GOTO(___L47_path_2d_trim)
#line 7
   ___END_IF
#line 7
   ___SET_R2(___STRINGREF(___R2,___FIX(0L)))
#line 7
   ___IF(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_)))
#line 7
   ___GOTO(___L25_path_2d_trim)
#line 7
   ___END_IF
#line 7
   ___GOTO(___L51_path_2d_trim)
#line 7
___DEF_SLBL(2,___L2_path_2d_trim)
#line 7
   ___SET_R2(___R1)
#line 7
   ___SET_R1(___STK(-2))
#line 7
   ___SET_R0(___STK(-3))
#line 7
   ___ADJFP(-4)
#line 7
   ___IF(___NOT(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_))))
#line 7
   ___GOTO(___L51_path_2d_trim)
#line 7
   ___END_IF
#line 7
___DEF_GLBL(___L25_path_2d_trim)
#line 7
   ___IF(___NOT(___EQP(___R2,___CHR(47))))
#line 7
   ___GOTO(___L29_path_2d_trim)
#line 7
   ___END_IF
#line 7
___DEF_GLBL(___L26_path_2d_trim)
#line 10
   ___SET_STK(1,___R0)
#line 10
   ___SET_STK(2,___R1)
   ___SET_R1(___UNBOX(___R1))
#line 11
   ___ADJFP(2)
#line 11
   ___IF(___NOT(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length))))
#line 11
   ___GOTO(___L42_path_2d_trim)
#line 11
   ___END_IF
#line 11
   ___IF(___NOT(___STRINGP(___R1)))
#line 11
   ___GOTO(___L42_path_2d_trim)
#line 11
   ___END_IF
#line 11
   ___SET_R1(___STRINGLENGTH(___R1))
#line 11
___DEF_GLBL(___L27_path_2d_trim)
#line 11
   ___SET_R3(___R1)
#line 10
   ___SET_R1(___UNBOX(___STK(0)))
   ___SET_R2(___FIX(1L))
#line 10
   ___SET_R0(___LBL(4))
#line 10
   ___ADJFP(2)
#line 10
   ___POLL(3)
#line 10
___DEF_SLBL(3,___L3_path_2d_trim)
#line 10
___DEF_GLBL(___L28_path_2d_trim)
#line 10
   ___JUMPGLOSAFE(___SET_NARGS(3),25,___G_substring)
#line 10
___DEF_SLBL(4,___L4_path_2d_trim)
#line 9
   ___SETBOX(___STK(-2),___R1)
#line 7
   ___SET_R1(___STK(-2))
#line 7
   ___SET_R0(___STK(-3))
#line 7
   ___ADJFP(-4)
#line 7
   ___GOTO(___L29_path_2d_trim)
#line 7
___DEF_SLBL(5,___L5_path_2d_trim)
#line 7
   ___IF(___NOT(___FALSEP(___R1)))
#line 7
   ___GOTO(___L50_path_2d_trim)
#line 7
   ___END_IF
#line 7
   ___SET_R1(___STK(-2))
#line 7
   ___SET_R0(___STK(-3))
#line 7
   ___ADJFP(-4)
#line 7
___DEF_GLBL(___L29_path_2d_trim)
#line 12
   ___SET_R2(___UNBOX(___R1))
#line 12
   ___SET_R3(___UNBOX(___R1))
#line 12
   ___IF(___NOT(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length))))
#line 12
   ___GOTO(___L49_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___IF(___NOT(___STRINGP(___R3)))
#line 12
   ___GOTO(___L49_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___SET_R3(___STRINGLENGTH(___R3))
#line 12
   ___IF(___NOT(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_))))
#line 12
   ___GOTO(___L31_path_2d_trim)
#line 12
   ___END_IF
#line 12
___DEF_GLBL(___L30_path_2d_trim)
#line 12
   ___IF(___NOT(___FIXNUMP(___R3)))
#line 12
   ___GOTO(___L31_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___SET_R4(___FIXSUBP(___R3,___FIX(1L)))
#line 12
   ___IF(___NOT(___FALSEP(___R4)))
#line 12
   ___GOTO(___L32_path_2d_trim)
#line 12
   ___END_IF
#line 12
___DEF_GLBL(___L31_path_2d_trim)
#line 12
   ___SET_STK(1,___R0)
#line 12
   ___SET_STK(2,___R1)
#line 12
   ___SET_STK(3,___R2)
#line 12
   ___SET_R1(___R3)
#line 12
   ___SET_R2(___FIX(1L))
#line 12
   ___SET_R0(___LBL(7))
#line 12
   ___ADJFP(4)
#line 12
   ___POLL(6)
#line 12
___DEF_SLBL(6,___L6_path_2d_trim)
#line 12
   ___JUMPGLOSAFE(___SET_NARGS(2),10,___G__2d_)
#line 12
___DEF_SLBL(7,___L7_path_2d_trim)
#line 12
   ___SET_R4(___R1)
#line 12
   ___SET_R2(___STK(-1))
#line 12
   ___SET_R1(___STK(-2))
#line 12
   ___SET_R0(___STK(-3))
#line 12
   ___ADJFP(-4)
#line 12
   ___IF(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref)))
#line 12
   ___GOTO(___L33_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___GOTO(___L46_path_2d_trim)
#line 12
___DEF_GLBL(___L32_path_2d_trim)
#line 12
   ___IF(___NOT(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref))))
#line 12
   ___GOTO(___L46_path_2d_trim)
#line 12
   ___END_IF
#line 12
___DEF_GLBL(___L33_path_2d_trim)
#line 12
   ___IF(___NOT(___STRINGP(___R2)))
#line 12
   ___GOTO(___L46_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___IF(___NOT(___FIXNUMP(___R4)))
#line 12
   ___GOTO(___L46_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___IF(___NOT(___FIXLE(___FIX(0L),___R4)))
#line 12
   ___GOTO(___L46_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___SET_R3(___STRINGLENGTH(___R2))
#line 12
   ___IF(___NOT(___FIXLT(___R4,___R3)))
#line 12
   ___GOTO(___L46_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___SET_R2(___STRINGREF(___R2,___R4))
#line 12
   ___IF(___NOT(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_))))
#line 12
   ___GOTO(___L44_path_2d_trim)
#line 12
   ___END_IF
#line 12
___DEF_GLBL(___L34_path_2d_trim)
#line 12
   ___IF(___NOT(___EQP(___R2,___CHR(47))))
#line 12
   ___GOTO(___L39_path_2d_trim)
#line 12
   ___END_IF
#line 12
___DEF_GLBL(___L35_path_2d_trim)
#line 15
   ___SET_STK(1,___R0)
#line 15
   ___SET_STK(2,___R1)
   ___SET_R1(___UNBOX(___R1))
#line 16
   ___ADJFP(2)
#line 16
   ___IF(___NOT(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length))))
#line 16
   ___GOTO(___L41_path_2d_trim)
#line 16
   ___END_IF
#line 16
   ___IF(___NOT(___STRINGP(___R1)))
#line 16
   ___GOTO(___L41_path_2d_trim)
#line 16
   ___END_IF
#line 16
   ___SET_R1(___STRINGLENGTH(___R1))
#line 16
   ___IF(___NOT(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_))))
#line 16
   ___GOTO(___L37_path_2d_trim)
#line 16
   ___END_IF
#line 16
___DEF_GLBL(___L36_path_2d_trim)
#line 16
   ___IF(___NOT(___FIXNUMP(___R1)))
#line 16
   ___GOTO(___L37_path_2d_trim)
#line 16
   ___END_IF
#line 16
   ___SET_R2(___FIXSUBP(___R1,___FIX(1L)))
#line 16
   ___IF(___NOT(___FALSEP(___R2)))
#line 16
   ___GOTO(___L38_path_2d_trim)
#line 16
   ___END_IF
#line 16
___DEF_GLBL(___L37_path_2d_trim)
#line 16
   ___SET_R2(___FIX(1L))
#line 16
   ___SET_R0(___LBL(9))
#line 16
   ___ADJFP(2)
#line 16
   ___POLL(8)
#line 16
___DEF_SLBL(8,___L8_path_2d_trim)
#line 16
   ___JUMPGLOSAFE(___SET_NARGS(2),10,___G__2d_)
#line 16
___DEF_SLBL(9,___L9_path_2d_trim)
#line 16
   ___SET_R2(___R1)
#line 16
   ___ADJFP(-2)
#line 16
___DEF_GLBL(___L38_path_2d_trim)
#line 16
   ___SET_R3(___R2)
#line 15
   ___SET_R1(___UNBOX(___STK(0)))
   ___SET_R2(___FIX(0L))
#line 15
   ___SET_R0(___LBL(11))
#line 15
   ___ADJFP(2)
#line 15
   ___POLL(10)
#line 15
___DEF_SLBL(10,___L10_path_2d_trim)
#line 15
   ___GOTO(___L28_path_2d_trim)
#line 15
___DEF_SLBL(11,___L11_path_2d_trim)
#line 14
   ___SETBOX(___STK(-2),___R1)
#line 12
   ___SET_R1(___STK(-2))
#line 12
   ___SET_R0(___STK(-3))
#line 12
   ___ADJFP(-4)
#line 12
   ___GOTO(___L39_path_2d_trim)
#line 12
___DEF_SLBL(12,___L12_path_2d_trim)
#line 12
   ___IF(___NOT(___FALSEP(___R1)))
#line 12
   ___GOTO(___L40_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___SET_R1(___STK(-2))
#line 12
   ___SET_R0(___STK(-3))
#line 12
   ___ADJFP(-4)
#line 12
___DEF_GLBL(___L39_path_2d_trim)
#line 17
   ___SET_R1(___UNBOX(___R1))
#line 17
   ___POLL(13)
#line 17
___DEF_SLBL(13,___L13_path_2d_trim)
#line 17
   ___JUMPPRM(___NOTHING,___R0)
#line 12
___DEF_GLBL(___L40_path_2d_trim)
#line 12
   ___SET_R1(___STK(-2))
#line 12
   ___SET_R0(___STK(-3))
#line 12
   ___ADJFP(-4)
#line 12
   ___GOTO(___L35_path_2d_trim)
#line 16
___DEF_GLBL(___L41_path_2d_trim)
#line 16
   ___SET_R0(___LBL(17))
#line 16
   ___ADJFP(2)
#line 16
   ___POLL(14)
#line 16
___DEF_SLBL(14,___L14_path_2d_trim)
#line 16
   ___GOTO(___L43_path_2d_trim)
#line 11
___DEF_GLBL(___L42_path_2d_trim)
#line 11
   ___SET_R0(___LBL(16))
#line 11
   ___ADJFP(2)
#line 11
   ___POLL(15)
#line 11
___DEF_SLBL(15,___L15_path_2d_trim)
#line 11
___DEF_GLBL(___L43_path_2d_trim)
#line 11
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_string_2d_length)
#line 11
___DEF_SLBL(16,___L16_path_2d_trim)
#line 11
   ___ADJFP(-2)
#line 11
   ___GOTO(___L27_path_2d_trim)
#line 16
___DEF_SLBL(17,___L17_path_2d_trim)
#line 16
   ___ADJFP(-2)
#line 16
   ___IF(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_)))
#line 16
   ___GOTO(___L36_path_2d_trim)
#line 16
   ___END_IF
#line 16
   ___GOTO(___L37_path_2d_trim)
#line 12
___DEF_SLBL(18,___L18_path_2d_trim)
#line 12
   ___SET_R2(___R1)
#line 12
   ___SET_R1(___STK(-2))
#line 12
   ___SET_R0(___STK(-3))
#line 12
   ___ADJFP(-4)
#line 12
   ___IF(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_)))
#line 12
   ___GOTO(___L34_path_2d_trim)
#line 12
   ___END_IF
#line 12
___DEF_GLBL(___L44_path_2d_trim)
#line 12
   ___SET_STK(1,___R0)
#line 12
   ___SET_STK(2,___R1)
#line 12
   ___SET_R1(___R2)
   ___SET_R2(___CHR(47))
#line 12
   ___SET_R0(___LBL(12))
#line 12
   ___ADJFP(4)
#line 12
   ___POLL(19)
#line 12
___DEF_SLBL(19,___L19_path_2d_trim)
#line 7
___DEF_GLBL(___L45_path_2d_trim)
#line 7
   ___JUMPGLOSAFE(___SET_NARGS(2),15,___G_eq_3f_)
#line 12
___DEF_GLBL(___L46_path_2d_trim)
#line 12
   ___SET_STK(1,___R0)
#line 12
   ___SET_STK(2,___R1)
#line 12
   ___SET_STK(3,___R2)
#line 12
   ___SET_R2(___R4)
#line 12
   ___SET_R1(___STK(3))
#line 12
   ___SET_R0(___LBL(18))
#line 12
   ___ADJFP(4)
#line 12
   ___POLL(20)
#line 12
___DEF_SLBL(20,___L20_path_2d_trim)
#line 12
   ___GOTO(___L48_path_2d_trim)
#line 7
___DEF_GLBL(___L47_path_2d_trim)
#line 7
   ___SET_STK(1,___R0)
#line 7
   ___SET_STK(2,___R1)
#line 7
   ___SET_R1(___R2)
#line 7
   ___SET_R2(___FIX(0L))
#line 7
   ___SET_R0(___LBL(2))
#line 7
   ___ADJFP(4)
#line 7
   ___POLL(21)
#line 7
___DEF_SLBL(21,___L21_path_2d_trim)
#line 7
___DEF_GLBL(___L48_path_2d_trim)
#line 7
   ___JUMPGLOSAFE(___SET_NARGS(2),24,___G_string_2d_ref)
#line 12
___DEF_GLBL(___L49_path_2d_trim)
#line 12
   ___SET_STK(1,___R0)
#line 12
   ___SET_STK(2,___R1)
#line 12
   ___SET_STK(3,___R2)
#line 12
   ___SET_R1(___R3)
#line 12
   ___SET_R0(___LBL(23))
#line 12
   ___ADJFP(4)
#line 12
   ___POLL(22)
#line 12
___DEF_SLBL(22,___L22_path_2d_trim)
#line 12
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_string_2d_length)
#line 12
___DEF_SLBL(23,___L23_path_2d_trim)
#line 12
   ___SET_R3(___R1)
#line 12
   ___SET_R2(___STK(-1))
#line 12
   ___SET_R1(___STK(-2))
#line 12
   ___SET_R0(___STK(-3))
#line 12
   ___ADJFP(-4)
#line 12
   ___IF(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_)))
#line 12
   ___GOTO(___L30_path_2d_trim)
#line 12
   ___END_IF
#line 12
   ___GOTO(___L31_path_2d_trim)
#line 7
___DEF_GLBL(___L50_path_2d_trim)
#line 7
   ___SET_R1(___STK(-2))
#line 7
   ___SET_R0(___STK(-3))
#line 7
   ___ADJFP(-4)
#line 7
   ___GOTO(___L26_path_2d_trim)
#line 7
___DEF_GLBL(___L51_path_2d_trim)
#line 7
   ___SET_STK(1,___R0)
#line 7
   ___SET_STK(2,___R1)
#line 7
   ___SET_R1(___R2)
   ___SET_R2(___CHR(47))
#line 7
   ___SET_R0(___LBL(5))
#line 7
   ___ADJFP(4)
#line 7
   ___POLL(24)
#line 7
___DEF_SLBL(24,___L24_path_2d_trim)
#line 7
   ___GOTO(___L45_path_2d_trim)
#line 971 "init.c"
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_join_2d_paths
#undef ___PH_LBL0
#define ___PH_LBL0 33
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_join_2d_paths)
___DEF_P_HLBL(___L1_join_2d_paths)
___DEF_P_HLBL(___L2_join_2d_paths)
___DEF_P_HLBL(___L3_join_2d_paths)
___DEF_P_HLBL(___L4_join_2d_paths)
___DEF_P_HLBL(___L5_join_2d_paths)
___DEF_P_HLBL(___L6_join_2d_paths)
___DEF_P_HLBL(___L7_join_2d_paths)
___DEF_P_HLBL(___L8_join_2d_paths)
___DEF_P_HLBL(___L9_join_2d_paths)
___DEF_P_HLBL(___L10_join_2d_paths)
___DEF_P_HLBL(___L11_join_2d_paths)
___DEF_P_HLBL(___L12_join_2d_paths)
___DEF_P_HLBL(___L13_join_2d_paths)
___DEF_P_HLBL(___L14_join_2d_paths)
___DEF_P_HLBL(___L15_join_2d_paths)
___DEF_P_HLBL(___L16_join_2d_paths)
___DEF_P_HLBL(___L17_join_2d_paths)
___DEF_P_HLBL(___L18_join_2d_paths)
___DEF_P_HLBL(___L19_join_2d_paths)
___DEF_P_HLBL(___L20_join_2d_paths)
___DEF_P_HLBL(___L21_join_2d_paths)
___DEF_P_HLBL(___L22_join_2d_paths)
___DEF_P_HLBL(___L23_join_2d_paths)
___DEF_P_HLBL(___L24_join_2d_paths)
___DEF_P_HLBL(___L25_join_2d_paths)
___DEF_P_HLBL(___L26_join_2d_paths)
___DEF_P_HLBL(___L27_join_2d_paths)
___DEF_P_HLBL(___L28_join_2d_paths)
___DEF_P_HLBL(___L29_join_2d_paths)
___DEF_P_HLBL(___L30_join_2d_paths)
___DEF_P_HLBL(___L31_join_2d_paths)
___DEF_P_HLBL(___L32_join_2d_paths)
___DEF_P_HLBL(___L33_join_2d_paths)
___DEF_P_HLBL(___L34_join_2d_paths)
___DEF_P_HLBL(___L35_join_2d_paths)
___DEF_P_HLBL(___L36_join_2d_paths)
___END_P_HLBL
___BEGIN_P_SW
#line 19 "resources.scm"
___DEF_SLBL(0,___L0_join_2d_paths)
#line 19
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
#line 19
   ___GET_REST(0,0,0,0)
#line 19
___DEF_GLBL(___L_join_2d_paths)
   ___IF(___NOT(___EQP(___GLO(12,___G_car),___PRM(12,___G_car))))
#line 20
   ___GOTO(___L89_join_2d_paths)
#line 20
   ___END_IF
#line 20
   ___IF(___NOT(___PAIRP(___R1)))
#line 20
   ___GOTO(___L89_join_2d_paths)
#line 20
   ___END_IF
#line 20
   ___SET_R2(___CAR(___R1))
#line 20
   ___IF(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref)))
#line 20
   ___GOTO(___L37_join_2d_paths)
#line 20
   ___END_IF
#line 20
   ___GOTO(___L88_join_2d_paths)
#line 20
___DEF_SLBL(1,___L1_join_2d_paths)
#line 20
   ___SET_R2(___R1)
#line 20
   ___SET_R1(___STK(-2))
#line 20
   ___SET_R0(___STK(-3))
#line 20
   ___ADJFP(-4)
#line 20
   ___IF(___NOT(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref))))
#line 20
   ___GOTO(___L88_join_2d_paths)
#line 20
   ___END_IF
#line 20
___DEF_GLBL(___L37_join_2d_paths)
#line 20
   ___IF(___NOT(___STRINGP(___R2)))
#line 20
   ___GOTO(___L88_join_2d_paths)
#line 20
   ___END_IF
#line 20
   ___SET_R3(___STRINGLENGTH(___R2))
#line 20
   ___IF(___NOT(___FIXLT(___FIX(0L),___R3)))
#line 20
   ___GOTO(___L88_join_2d_paths)
#line 20
   ___END_IF
#line 20
   ___SET_R2(___STRINGREF(___R2,___FIX(0L)))
#line 20
   ___IF(___NOT(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_))))
#line 20
   ___GOTO(___L87_join_2d_paths)
#line 20
   ___END_IF
#line 20
___DEF_GLBL(___L38_join_2d_paths)
#line 20
   ___IF(___NOT(___EQP(___R2,___CHR(47))))
#line 20
   ___GOTO(___L84_join_2d_paths)
#line 20
   ___END_IF
#line 20
___DEF_GLBL(___L39_join_2d_paths)
   ___SET_R2(___SUB(3))
   ___IF(___NOT(___EQP(___GLO(12,___G_car),___PRM(12,___G_car))))
#line 22
   ___GOTO(___L85_join_2d_paths)
#line 22
   ___END_IF
#line 22
___DEF_GLBL(___L40_join_2d_paths)
#line 22
   ___IF(___NOT(___PAIRP(___R1)))
#line 22
   ___GOTO(___L85_join_2d_paths)
#line 22
   ___END_IF
#line 22
   ___SET_R3(___CAR(___R1))
   ___IF(___NOT(___EQP(___GLO(13,___G_cdr),___PRM(13,___G_cdr))))
#line 23
   ___GOTO(___L83_join_2d_paths)
#line 23
   ___END_IF
#line 23
___DEF_GLBL(___L41_join_2d_paths)
#line 23
   ___IF(___NOT(___PAIRP(___R1)))
#line 23
   ___GOTO(___L83_join_2d_paths)
#line 23
   ___END_IF
#line 23
   ___SET_R1(___CDR(___R1))
#line 23
___DEF_GLBL(___L42_join_2d_paths)
   ___SET_STK(1,___R0)
#line 24
   ___SET_STK(2,___R1)
#line 24
   ___SET_STK(3,___R3)
#line 24
   ___SET_STK(4,___R2)
#line 24
   ___SET_R1(___R3)
#line 24
   ___SET_R0(___LBL(3))
#line 24
   ___ADJFP(8)
#line 24
   ___POLL(2)
#line 24
___DEF_SLBL(2,___L2_join_2d_paths)
#line 24
___DEF_GLBL(___L43_join_2d_paths)
#line 24
   ___JUMPGLOSAFE(___SET_NARGS(1),5,___G_path_2d_trim)
#line 24
___DEF_SLBL(3,___L3_join_2d_paths)
#line 24
   ___SET_R2(___R1)
#line 24
   ___SET_R1(___STK(-4))
#line 24
   ___SET_R3(___SUB(4))
#line 24
   ___SET_R0(___LBL(4))
#line 24
   ___ADJFP(-4)
#line 24
   ___JUMPGLOSAFE(___SET_NARGS(3),22,___G_string_2d_append)
#line 24
___DEF_SLBL(4,___L4_join_2d_paths)
   ___IF(___NOT(___EQP(___GLO(19,___G_null_3f_),___PRM(19,___G_null_3f_))))
#line 25
   ___GOTO(___L82_join_2d_paths)
#line 25
   ___END_IF
#line 25
   ___IF(___NOT(___NULLP(___STK(-2))))
#line 25
   ___GOTO(___L76_join_2d_paths)
#line 25
   ___END_IF
   ___IF(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length)))
#line 26
   ___GOTO(___L44_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___GOTO(___L74_join_2d_paths)
#line 25
___DEF_SLBL(5,___L5_join_2d_paths)
#line 25
   ___IF(___FALSEP(___R1))
#line 25
   ___GOTO(___L75_join_2d_paths)
#line 25
   ___END_IF
#line 25
   ___SET_R1(___STK(-4))
   ___ADJFP(-4)
#line 26
   ___IF(___NOT(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length))))
#line 26
   ___GOTO(___L74_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L44_join_2d_paths)
#line 26
   ___IF(___NOT(___STRINGP(___STK(-1))))
#line 26
   ___GOTO(___L74_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R2(___STRINGLENGTH(___STK(-1)))
#line 26
   ___IF(___NOT(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_))))
#line 26
   ___GOTO(___L73_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L45_join_2d_paths)
#line 26
   ___IF(___NOT(___FIXNUMP(___R2)))
#line 26
   ___GOTO(___L73_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R3(___FIXSUBP(___R2,___FIX(1L)))
#line 26
   ___IF(___FALSEP(___R3))
#line 26
   ___GOTO(___L73_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___IF(___NOT(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref))))
#line 26
   ___GOTO(___L72_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L46_join_2d_paths)
#line 26
   ___IF(___NOT(___STRINGP(___STK(-1))))
#line 26
   ___GOTO(___L72_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___IF(___NOT(___FIXNUMP(___R3)))
#line 26
   ___GOTO(___L72_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___IF(___NOT(___FIXLE(___FIX(0L),___R3)))
#line 26
   ___GOTO(___L72_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R2(___STRINGLENGTH(___STK(-1)))
#line 26
   ___IF(___NOT(___FIXLT(___R3,___R2)))
#line 26
   ___GOTO(___L72_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R2(___STRINGREF(___STK(-1),___R3))
#line 26
   ___IF(___NOT(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_))))
#line 26
   ___GOTO(___L58_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L47_join_2d_paths)
#line 26
   ___IF(___NOT(___EQP(___R2,___CHR(47))))
#line 26
   ___GOTO(___L49_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___POLL(6)
#line 26
___DEF_SLBL(6,___L6_join_2d_paths)
#line 26
___DEF_GLBL(___L48_join_2d_paths)
#line 28
   ___ADJFP(-4)
#line 28
   ___JUMPPRM(___NOTHING,___STK(1))
#line 26
___DEF_GLBL(___L49_join_2d_paths)
#line 29
   ___IF(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length)))
#line 29
   ___GOTO(___L50_join_2d_paths)
#line 29
   ___END_IF
#line 29
   ___GOTO(___L54_join_2d_paths)
#line 26
___DEF_SLBL(7,___L7_join_2d_paths)
#line 26
   ___IF(___NOT(___FALSEP(___R1)))
#line 26
   ___GOTO(___L55_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R1(___STK(-2))
#line 29
   ___IF(___NOT(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length))))
#line 29
   ___GOTO(___L54_join_2d_paths)
#line 29
   ___END_IF
#line 29
___DEF_GLBL(___L50_join_2d_paths)
#line 29
   ___IF(___NOT(___STRINGP(___R1)))
#line 29
   ___GOTO(___L54_join_2d_paths)
#line 29
   ___END_IF
#line 29
   ___SET_R2(___STRINGLENGTH(___R1))
#line 29
   ___IF(___NOT(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_))))
#line 29
   ___GOTO(___L53_join_2d_paths)
#line 29
   ___END_IF
#line 29
___DEF_GLBL(___L51_join_2d_paths)
#line 29
   ___IF(___NOT(___FIXNUMP(___R2)))
#line 29
   ___GOTO(___L53_join_2d_paths)
#line 29
   ___END_IF
#line 29
   ___SET_R3(___FIXSUBP(___R2,___FIX(1L)))
#line 29
   ___IF(___FALSEP(___R3))
#line 29
   ___GOTO(___L53_join_2d_paths)
#line 29
   ___END_IF
#line 29
___DEF_GLBL(___L52_join_2d_paths)
#line 29
   ___SET_R2(___FIX(0L))
#line 29
   ___SET_R0(___STK(-3))
#line 29
   ___POLL(8)
#line 29
___DEF_SLBL(8,___L8_join_2d_paths)
#line 29
   ___ADJFP(-4)
#line 29
   ___JUMPGLOSAFE(___SET_NARGS(3),25,___G_substring)
#line 29
___DEF_SLBL(9,___L9_join_2d_paths)
#line 29
   ___SET_R2(___R1)
#line 29
   ___SET_R1(___STK(-2))
#line 29
   ___IF(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_)))
#line 29
   ___GOTO(___L51_join_2d_paths)
#line 29
   ___END_IF
#line 29
___DEF_GLBL(___L53_join_2d_paths)
#line 29
   ___SET_STK(-2,___R1)
#line 29
   ___SET_R1(___R2)
#line 29
   ___SET_R2(___FIX(1L))
#line 29
   ___SET_R0(___LBL(10))
#line 29
   ___JUMPGLOSAFE(___SET_NARGS(2),10,___G__2d_)
#line 29
___DEF_SLBL(10,___L10_join_2d_paths)
#line 29
   ___SET_R3(___R1)
#line 29
   ___SET_R1(___STK(-2))
#line 29
   ___GOTO(___L52_join_2d_paths)
#line 29
___DEF_GLBL(___L54_join_2d_paths)
#line 29
   ___SET_STK(-2,___R1)
#line 29
   ___SET_R0(___LBL(9))
#line 29
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_string_2d_length)
#line 26
___DEF_GLBL(___L55_join_2d_paths)
#line 26
   ___SET_R1(___STK(-2))
#line 26
   ___POLL(11)
#line 26
___DEF_SLBL(11,___L11_join_2d_paths)
#line 26
   ___GOTO(___L48_join_2d_paths)
#line 26
___DEF_SLBL(12,___L12_join_2d_paths)
#line 26
   ___SET_R2(___R1)
#line 26
   ___SET_R1(___STK(-1))
#line 26
   ___IF(___NOT(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_))))
#line 26
   ___GOTO(___L60_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L56_join_2d_paths)
#line 26
   ___IF(___NOT(___FIXNUMP(___R2)))
#line 26
   ___GOTO(___L60_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R3(___FIXSUBP(___R2,___FIX(1L)))
#line 26
   ___IF(___FALSEP(___R3))
#line 26
   ___GOTO(___L60_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___IF(___NOT(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref))))
#line 26
   ___GOTO(___L61_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L57_join_2d_paths)
#line 26
   ___IF(___NOT(___STRINGP(___STK(-2))))
#line 26
   ___GOTO(___L61_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___IF(___NOT(___FIXNUMP(___R3)))
#line 26
   ___GOTO(___L61_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___IF(___NOT(___FIXLE(___FIX(0L),___R3)))
#line 26
   ___GOTO(___L61_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R2(___STRINGLENGTH(___STK(-2)))
#line 26
   ___IF(___NOT(___FIXLT(___R3,___R2)))
#line 26
   ___GOTO(___L61_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R2(___STRINGREF(___STK(-2),___R3))
#line 26
   ___IF(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_)))
#line 26
   ___GOTO(___L47_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L58_join_2d_paths)
#line 26
   ___SET_STK(-2,___R1)
#line 26
   ___SET_R1(___R2)
   ___SET_R2(___CHR(47))
#line 26
   ___SET_R0(___LBL(7))
#line 26
   ___JUMPGLOSAFE(___SET_NARGS(2),15,___G_eq_3f_)
#line 25
___DEF_SLBL(13,___L13_join_2d_paths)
#line 25
   ___IF(___FALSEP(___R1))
#line 25
   ___GOTO(___L62_join_2d_paths)
#line 25
   ___END_IF
#line 25
   ___SET_R1(___STK(-4))
   ___ADJFP(-4)
#line 26
   ___IF(___NOT(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length))))
#line 26
   ___GOTO(___L71_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L59_join_2d_paths)
#line 26
   ___IF(___NOT(___STRINGP(___STK(-2))))
#line 26
   ___GOTO(___L71_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___SET_R2(___STRINGLENGTH(___STK(-2)))
#line 26
   ___IF(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_)))
#line 26
   ___GOTO(___L56_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L60_join_2d_paths)
#line 26
   ___SET_STK(-1,___R1)
#line 26
   ___SET_R1(___R2)
#line 26
   ___SET_R2(___FIX(1L))
#line 26
   ___SET_R0(___LBL(14))
#line 26
   ___JUMPGLOSAFE(___SET_NARGS(2),10,___G__2d_)
#line 26
___DEF_SLBL(14,___L14_join_2d_paths)
#line 26
   ___SET_R3(___R1)
#line 26
   ___SET_R1(___STK(-1))
#line 26
   ___IF(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref)))
#line 26
   ___GOTO(___L57_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L61_join_2d_paths)
#line 26
   ___SET_STK(-1,___R1)
#line 26
   ___SET_R2(___R3)
#line 26
   ___SET_R1(___STK(-2))
#line 26
   ___SET_R0(___LBL(15))
#line 26
   ___JUMPGLOSAFE(___SET_NARGS(2),24,___G_string_2d_ref)
#line 26
___DEF_SLBL(15,___L15_join_2d_paths)
#line 26
   ___SET_R2(___R1)
#line 26
   ___SET_R1(___STK(-1))
#line 26
   ___IF(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_)))
#line 26
   ___GOTO(___L47_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___GOTO(___L58_join_2d_paths)
#line 25
___DEF_GLBL(___L62_join_2d_paths)
#line 25
   ___SET_R1(___STK(-4))
#line 31
   ___ADJFP(-4)
#line 31
   ___IF(___EQP(___GLO(12,___G_car),___PRM(12,___G_car)))
#line 31
   ___GOTO(___L63_join_2d_paths)
#line 31
   ___END_IF
#line 31
   ___GOTO(___L68_join_2d_paths)
#line 24
___DEF_SLBL(16,___L16_join_2d_paths)
   ___IF(___NOT(___EQP(___GLO(19,___G_null_3f_),___PRM(19,___G_null_3f_))))
#line 25
   ___GOTO(___L69_join_2d_paths)
#line 25
   ___END_IF
#line 25
   ___IF(___NULLP(___STK(-1)))
#line 25
   ___GOTO(___L70_join_2d_paths)
#line 25
   ___END_IF
#line 31
   ___IF(___NOT(___EQP(___GLO(12,___G_car),___PRM(12,___G_car))))
#line 31
   ___GOTO(___L68_join_2d_paths)
#line 31
   ___END_IF
#line 31
___DEF_GLBL(___L63_join_2d_paths)
#line 31
   ___IF(___NOT(___PAIRP(___STK(-1))))
#line 31
   ___GOTO(___L68_join_2d_paths)
#line 31
   ___END_IF
#line 31
   ___SET_R2(___CAR(___STK(-1)))
#line 31
___DEF_GLBL(___L64_join_2d_paths)
#line 31
   ___SET_STK(-2,___R2)
   ___IF(___NOT(___EQP(___GLO(13,___G_cdr),___PRM(13,___G_cdr))))
#line 32
   ___GOTO(___L67_join_2d_paths)
#line 32
   ___END_IF
#line 32
   ___IF(___NOT(___PAIRP(___STK(-1))))
#line 32
   ___GOTO(___L67_join_2d_paths)
#line 32
   ___END_IF
#line 32
   ___SET_R2(___CDR(___STK(-1)))
#line 32
___DEF_GLBL(___L65_join_2d_paths)
#line 32
   ___SET_R3(___R2)
#line 30
   ___SET_R0(___STK(-3))
#line 30
   ___SET_R2(___STK(-2))
#line 30
   ___ADJFP(-4)
#line 30
   ___POLL(17)
#line 30
___DEF_SLBL(17,___L17_join_2d_paths)
#line 20
___DEF_GLBL(___L66_join_2d_paths)
#line 24
   ___SET_STK(1,___R0)
#line 24
   ___SET_STK(2,___R2)
#line 24
   ___SET_STK(3,___R3)
#line 24
   ___SET_STK(4,___R1)
#line 24
   ___SET_R1(___R2)
#line 24
   ___SET_R0(___LBL(19))
#line 24
   ___ADJFP(8)
#line 24
   ___POLL(18)
#line 24
___DEF_SLBL(18,___L18_join_2d_paths)
#line 24
   ___GOTO(___L43_join_2d_paths)
#line 24
___DEF_SLBL(19,___L19_join_2d_paths)
#line 24
   ___SET_R2(___R1)
#line 24
   ___SET_R1(___STK(-4))
#line 24
   ___SET_R3(___SUB(4))
#line 24
   ___SET_R0(___LBL(16))
#line 24
   ___ADJFP(-4)
#line 24
   ___JUMPGLOSAFE(___SET_NARGS(3),22,___G_string_2d_append)
#line 32
___DEF_GLBL(___L67_join_2d_paths)
#line 32
   ___SET_STK(0,___R1)
#line 32
   ___SET_R1(___STK(-1))
#line 32
   ___SET_R0(___LBL(20))
#line 32
   ___ADJFP(4)
#line 32
   ___JUMPGLOSAFE(___SET_NARGS(1),13,___G_cdr)
#line 32
___DEF_SLBL(20,___L20_join_2d_paths)
#line 32
   ___SET_R2(___R1)
#line 32
   ___SET_R1(___STK(-4))
#line 32
   ___ADJFP(-4)
#line 32
   ___GOTO(___L65_join_2d_paths)
#line 31
___DEF_GLBL(___L68_join_2d_paths)
#line 31
   ___SET_STK(-2,___R1)
#line 31
   ___SET_R1(___STK(-1))
#line 31
   ___SET_R0(___LBL(21))
#line 31
   ___JUMPGLOSAFE(___SET_NARGS(1),12,___G_car)
#line 31
___DEF_SLBL(21,___L21_join_2d_paths)
#line 31
   ___SET_R2(___R1)
#line 31
   ___SET_R1(___STK(-2))
#line 31
   ___GOTO(___L64_join_2d_paths)
#line 25
___DEF_GLBL(___L69_join_2d_paths)
#line 25
   ___SET_STK(0,___R1)
#line 25
   ___SET_R1(___STK(-1))
#line 25
   ___SET_R0(___LBL(13))
#line 25
   ___ADJFP(4)
#line 25
   ___JUMPGLOSAFE(___SET_NARGS(1),19,___G_null_3f_)
#line 25
___DEF_GLBL(___L70_join_2d_paths)
   ___IF(___EQP(___GLO(23,___G_string_2d_length),___PRM(23,___G_string_2d_length)))
#line 26
   ___GOTO(___L59_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L71_join_2d_paths)
#line 26
   ___SET_STK(-1,___R1)
#line 26
   ___SET_R1(___STK(-2))
#line 26
   ___SET_R0(___LBL(12))
#line 26
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_string_2d_length)
#line 26
___DEF_SLBL(22,___L22_join_2d_paths)
#line 26
   ___SET_R3(___R1)
#line 26
   ___SET_R1(___STK(-2))
#line 26
   ___IF(___EQP(___GLO(24,___G_string_2d_ref),___PRM(24,___G_string_2d_ref)))
#line 26
   ___GOTO(___L46_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L72_join_2d_paths)
#line 26
   ___SET_STK(-2,___R1)
#line 26
   ___SET_R2(___R3)
#line 26
   ___SET_R1(___STK(-1))
#line 26
   ___SET_R0(___LBL(23))
#line 26
   ___JUMPGLOSAFE(___SET_NARGS(2),24,___G_string_2d_ref)
#line 26
___DEF_SLBL(23,___L23_join_2d_paths)
#line 26
   ___SET_R2(___R1)
#line 26
   ___SET_R1(___STK(-2))
#line 26
   ___IF(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_)))
#line 26
   ___GOTO(___L47_join_2d_paths)
#line 26
   ___END_IF
#line 26
   ___GOTO(___L58_join_2d_paths)
#line 26
___DEF_SLBL(24,___L24_join_2d_paths)
#line 26
   ___SET_R2(___R1)
#line 26
   ___SET_R1(___STK(-2))
#line 26
   ___IF(___EQP(___GLO(10,___G__2d_),___PRM(10,___G__2d_)))
#line 26
   ___GOTO(___L45_join_2d_paths)
#line 26
   ___END_IF
#line 26
___DEF_GLBL(___L73_join_2d_paths)
#line 26
   ___SET_STK(-2,___R1)
#line 26
   ___SET_R1(___R2)
#line 26
   ___SET_R2(___FIX(1L))
#line 26
   ___SET_R0(___LBL(22))
#line 26
   ___JUMPGLOSAFE(___SET_NARGS(2),10,___G__2d_)
#line 26
___DEF_GLBL(___L74_join_2d_paths)
#line 26
   ___SET_STK(-2,___R1)
#line 26
   ___SET_R1(___STK(-1))
#line 26
   ___SET_R0(___LBL(24))
#line 26
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_string_2d_length)
#line 25
___DEF_GLBL(___L75_join_2d_paths)
#line 25
   ___SET_R1(___STK(-4))
#line 31
   ___ADJFP(-4)
#line 31
   ___IF(___EQP(___GLO(12,___G_car),___PRM(12,___G_car)))
#line 31
   ___GOTO(___L77_join_2d_paths)
#line 31
   ___END_IF
#line 31
   ___GOTO(___L81_join_2d_paths)
#line 25
___DEF_GLBL(___L76_join_2d_paths)
#line 31
   ___IF(___NOT(___EQP(___GLO(12,___G_car),___PRM(12,___G_car))))
#line 31
   ___GOTO(___L81_join_2d_paths)
#line 31
   ___END_IF
#line 31
___DEF_GLBL(___L77_join_2d_paths)
#line 31
   ___IF(___NOT(___PAIRP(___STK(-2))))
#line 31
   ___GOTO(___L81_join_2d_paths)
#line 31
   ___END_IF
#line 31
   ___SET_R2(___CAR(___STK(-2)))
#line 31
___DEF_GLBL(___L78_join_2d_paths)
#line 31
   ___SET_STK(-1,___R2)
   ___IF(___NOT(___EQP(___GLO(13,___G_cdr),___PRM(13,___G_cdr))))
#line 32
   ___GOTO(___L80_join_2d_paths)
#line 32
   ___END_IF
#line 32
   ___IF(___NOT(___PAIRP(___STK(-2))))
#line 32
   ___GOTO(___L80_join_2d_paths)
#line 32
   ___END_IF
#line 32
   ___SET_R2(___CDR(___STK(-2)))
#line 32
___DEF_GLBL(___L79_join_2d_paths)
#line 32
   ___SET_R3(___R2)
#line 30
   ___SET_R0(___STK(-3))
#line 30
   ___SET_R2(___STK(-1))
#line 30
   ___ADJFP(-4)
#line 30
   ___POLL(25)
#line 30
___DEF_SLBL(25,___L25_join_2d_paths)
#line 30
   ___GOTO(___L66_join_2d_paths)
#line 32
___DEF_GLBL(___L80_join_2d_paths)
#line 32
   ___SET_STK(0,___R1)
#line 32
   ___SET_R1(___STK(-2))
#line 32
   ___SET_R0(___LBL(26))
#line 32
   ___ADJFP(4)
#line 32
   ___JUMPGLOSAFE(___SET_NARGS(1),13,___G_cdr)
#line 32
___DEF_SLBL(26,___L26_join_2d_paths)
#line 32
   ___SET_R2(___R1)
#line 32
   ___SET_R1(___STK(-4))
#line 32
   ___ADJFP(-4)
#line 32
   ___GOTO(___L79_join_2d_paths)
#line 31
___DEF_GLBL(___L81_join_2d_paths)
#line 31
   ___SET_STK(-1,___R1)
#line 31
   ___SET_R1(___STK(-2))
#line 31
   ___SET_R0(___LBL(27))
#line 31
   ___JUMPGLOSAFE(___SET_NARGS(1),12,___G_car)
#line 31
___DEF_SLBL(27,___L27_join_2d_paths)
#line 31
   ___SET_R2(___R1)
#line 31
   ___SET_R1(___STK(-1))
#line 31
   ___GOTO(___L78_join_2d_paths)
#line 25
___DEF_GLBL(___L82_join_2d_paths)
#line 25
   ___SET_STK(0,___R1)
#line 25
   ___SET_R1(___STK(-2))
#line 25
   ___SET_R0(___LBL(5))
#line 25
   ___ADJFP(4)
#line 25
   ___JUMPGLOSAFE(___SET_NARGS(1),19,___G_null_3f_)
#line 22
___DEF_SLBL(28,___L28_join_2d_paths)
#line 22
   ___SET_R3(___R1)
#line 22
   ___SET_R2(___STK(-1))
#line 22
   ___SET_R1(___STK(-2))
#line 22
   ___SET_R0(___STK(-3))
   ___ADJFP(-4)
#line 23
   ___IF(___EQP(___GLO(13,___G_cdr),___PRM(13,___G_cdr)))
#line 23
   ___GOTO(___L41_join_2d_paths)
#line 23
   ___END_IF
#line 23
___DEF_GLBL(___L83_join_2d_paths)
#line 23
   ___SET_STK(1,___R0)
#line 23
   ___SET_STK(2,___R2)
#line 23
   ___SET_STK(3,___R3)
#line 23
   ___SET_R0(___LBL(30))
#line 23
   ___ADJFP(4)
#line 23
   ___POLL(29)
#line 23
___DEF_SLBL(29,___L29_join_2d_paths)
#line 23
   ___JUMPGLOSAFE(___SET_NARGS(1),13,___G_cdr)
#line 23
___DEF_SLBL(30,___L30_join_2d_paths)
#line 23
   ___SET_R3(___STK(-1))
#line 23
   ___SET_R2(___STK(-2))
#line 23
   ___SET_R0(___STK(-3))
#line 23
   ___ADJFP(-4)
#line 23
   ___GOTO(___L42_join_2d_paths)
#line 20
___DEF_SLBL(31,___L31_join_2d_paths)
#line 20
   ___IF(___NOT(___FALSEP(___R1)))
#line 20
   ___GOTO(___L86_join_2d_paths)
#line 20
   ___END_IF
#line 20
   ___SET_R1(___STK(-2))
#line 20
   ___SET_R0(___STK(-3))
#line 20
   ___ADJFP(-4)
#line 20
___DEF_GLBL(___L84_join_2d_paths)
   ___SET_R2(___SUB(5))
   ___IF(___EQP(___GLO(12,___G_car),___PRM(12,___G_car)))
#line 22
   ___GOTO(___L40_join_2d_paths)
#line 22
   ___END_IF
#line 22
___DEF_GLBL(___L85_join_2d_paths)
#line 22
   ___SET_STK(1,___R0)
#line 22
   ___SET_STK(2,___R1)
#line 22
   ___SET_STK(3,___R2)
#line 22
   ___SET_R0(___LBL(28))
#line 22
   ___ADJFP(4)
#line 22
   ___POLL(32)
#line 22
___DEF_SLBL(32,___L32_join_2d_paths)
#line 22
   ___JUMPGLOSAFE(___SET_NARGS(1),12,___G_car)
#line 20
___DEF_GLBL(___L86_join_2d_paths)
#line 20
   ___SET_R1(___STK(-2))
#line 20
   ___SET_R0(___STK(-3))
#line 20
   ___ADJFP(-4)
#line 20
   ___GOTO(___L39_join_2d_paths)
#line 20
___DEF_SLBL(33,___L33_join_2d_paths)
#line 20
   ___SET_R2(___R1)
#line 20
   ___SET_R1(___STK(-2))
#line 20
   ___SET_R0(___STK(-3))
#line 20
   ___ADJFP(-4)
#line 20
   ___IF(___EQP(___GLO(15,___G_eq_3f_),___PRM(15,___G_eq_3f_)))
#line 20
   ___GOTO(___L38_join_2d_paths)
#line 20
   ___END_IF
#line 20
___DEF_GLBL(___L87_join_2d_paths)
#line 20
   ___SET_STK(1,___R0)
#line 20
   ___SET_STK(2,___R1)
#line 20
   ___SET_R1(___R2)
#line 20
   ___SET_R2(___CHR(47))
#line 20
   ___SET_R0(___LBL(31))
#line 20
   ___ADJFP(4)
#line 20
   ___POLL(34)
#line 20
___DEF_SLBL(34,___L34_join_2d_paths)
#line 20
   ___JUMPGLOSAFE(___SET_NARGS(2),15,___G_eq_3f_)
#line 20
___DEF_GLBL(___L88_join_2d_paths)
#line 20
   ___SET_STK(1,___R0)
#line 20
   ___SET_STK(2,___R1)
#line 20
   ___SET_R1(___R2)
#line 20
   ___SET_R2(___FIX(0L))
#line 20
   ___SET_R0(___LBL(33))
#line 20
   ___ADJFP(4)
#line 20
   ___POLL(35)
#line 20
___DEF_SLBL(35,___L35_join_2d_paths)
#line 20
   ___JUMPGLOSAFE(___SET_NARGS(2),24,___G_string_2d_ref)
#line 20
___DEF_GLBL(___L89_join_2d_paths)
#line 20
   ___SET_STK(1,___R0)
#line 20
   ___SET_STK(2,___R1)
#line 20
   ___SET_R0(___LBL(1))
#line 20
   ___ADJFP(4)
#line 20
   ___POLL(36)
#line 20
___DEF_SLBL(36,___L36_join_2d_paths)
#line 20
   ___JUMPGLOSAFE(___SET_NARGS(1),12,___G_car)
#line 2126 "init.c"
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_resource
#undef ___PH_LBL0
#define ___PH_LBL0 71
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_resource)
___DEF_P_HLBL(___L1_resource)
___DEF_P_HLBL(___L2_resource)
___DEF_P_HLBL(___L3_resource)
___DEF_P_HLBL(___L4_resource)
___END_P_HLBL
___BEGIN_P_SW
#line 34 "resources.scm"
___DEF_SLBL(0,___L0_resource)
#line 34
   ___IF_NARGS_EQ(0,___SET_R1(___NUL))
#line 34
   ___GET_REST(0,0,0,0)
#line 34
___DEF_GLBL(___L_resource)
#line 37
   ___IF(___NOT(___EQP(___GLO(14,___G_cons),___PRM(14,___G_cons))))
#line 37
   ___GOTO(___L6_resource)
#line 37
   ___END_IF
#line 37
   ___SET_R1(___CONS(___SUB(6),___R1))
#line 37
   ___CHECK_HEAP(1,4096)
#line 37
___DEF_SLBL(1,___L1_resource)
#line 37
   ___GOTO(___L5_resource)
#line 37
___DEF_SLBL(2,___L2_resource)
#line 37
   ___SET_R0(___STK(-3))
#line 37
   ___ADJFP(-4)
#line 37
___DEF_GLBL(___L5_resource)
#line 37
   ___SET_R2(___R1)
#line 36
   ___SET_R1(___GLO(3,___G_join_2d_paths))
#line 36
   ___POLL(3)
#line 36
___DEF_SLBL(3,___L3_resource)
#line 35
   ___JUMPGLOSAFE(___SET_NARGS(2),11,___G_apply)
#line 37
___DEF_GLBL(___L6_resource)
#line 37
   ___SET_STK(1,___R0)
#line 37
   ___SET_R2(___R1)
#line 37
   ___SET_R1(___SUB(6))
#line 37
   ___SET_R0(___LBL(2))
#line 37
   ___ADJFP(4)
#line 37
   ___POLL(4)
#line 37
___DEF_SLBL(4,___L4_resource)
#line 37
   ___JUMPGLOSAFE(___SET_NARGS(2),14,___G_cons)
#line 2208 "init.c"
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_init_2d_opengl_2d_c
#undef ___PH_LBL0
#define ___PH_LBL0 77
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_init_2d_opengl_2d_c)
___DEF_P_HLBL(___L1_init_2d_opengl_2d_c)
___END_P_HLBL
___BEGIN_P_SW
#line 9 "init.scm"
___DEF_SLBL(0,___L0_init_2d_opengl_2d_c)
#line 9
   ___IF_NARGS_EQ(0,___NOTHING)
#line 9
   ___WRONG_NARGS(0,0,0,0)
#line 9
___DEF_GLBL(___L_init_2d_opengl_2d_c)
#line 9
   ___POLL(1)
#line 9
___DEF_SLBL(1,___L1_init_2d_opengl_2d_c)
   ___JUMPGLOSAFE(___SET_NARGS(0),17,___G_init_2d_opengl)
#line 2242 "init.c"
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_init_2d_engine_2d_c
#undef ___PH_LBL0
#define ___PH_LBL0 80
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_init_2d_engine_2d_c)
___DEF_P_HLBL(___L1_init_2d_engine_2d_c)
___END_P_HLBL
___BEGIN_P_SW
#line 12 "init.scm"
___DEF_SLBL(0,___L0_init_2d_engine_2d_c)
#line 12
   ___IF_NARGS_EQ(2,___NOTHING)
#line 12
   ___WRONG_NARGS(0,2,0,0)
#line 12
___DEF_GLBL(___L_init_2d_engine_2d_c)
#line 12
   ___POLL(1)
#line 12
___DEF_SLBL(1,___L1_init_2d_engine_2d_c)
#line 14
   ___JUMPGLOSAFE(___SET_NARGS(2),16,___G_init_2d_engine)
#line 2277 "init.c"
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_shutdown_2d_engine_2d_c
#undef ___PH_LBL0
#define ___PH_LBL0 83
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_shutdown_2d_engine_2d_c)
___DEF_P_HLBL(___L1_shutdown_2d_engine_2d_c)
___END_P_HLBL
___BEGIN_P_SW
#line 16 "init.scm"
___DEF_SLBL(0,___L0_shutdown_2d_engine_2d_c)
#line 16
   ___IF_NARGS_EQ(0,___NOTHING)
#line 16
   ___WRONG_NARGS(0,0,0,0)
#line 16
___DEF_GLBL(___L_shutdown_2d_engine_2d_c)
#line 16
   ___POLL(1)
#line 16
___DEF_SLBL(1,___L1_shutdown_2d_engine_2d_c)
   ___JUMPGLOSAFE(___SET_NARGS(0),21,___G_shutdown_2d_engine)
#line 2311 "init.c"
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_run_2d_frame_2d_c
#undef ___PH_LBL0
#define ___PH_LBL0 86
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_run_2d_frame_2d_c)
___DEF_P_HLBL(___L1_run_2d_frame_2d_c)
___END_P_HLBL
___BEGIN_P_SW
#line 19 "init.scm"
___DEF_SLBL(0,___L0_run_2d_frame_2d_c)
#line 19
   ___IF_NARGS_EQ(0,___NOTHING)
#line 19
   ___WRONG_NARGS(0,0,0,0)
#line 19
___DEF_GLBL(___L_run_2d_frame_2d_c)
#line 19
   ___POLL(1)
#line 19
___DEF_SLBL(1,___L1_run_2d_frame_2d_c)
   ___JUMPGLOSAFE(___SET_NARGS(0),20,___G_run_2d_frame)
#line 2345 "init.c"
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_init," init",___REF_FAL,5,0)
,___DEF_LBL_PROC(___H__20_init,0,0)
,___DEF_LBL_RET(___H__20_init,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__20_init,___IFD(___RETN,3,0,0x1L))
,___DEF_LBL_RET(___H__20_init,___IFD(___RETN,3,0,0x1L))
,___DEF_LBL_RET(___H__20_init,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_INTRO(___H_path_2d_trim,0,___REF_FAL,25,0)
,___DEF_LBL_PROC(___H_path_2d_trim,1,0)
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f7L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f7L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_path_2d_trim,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_INTRO(___H_join_2d_paths,0,___REF_FAL,37,0)
,___DEF_LBL_PROC(___H_join_2d_paths,1,0)
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,7,0,0xfL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,7,0,0xfL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,7,0,0xfL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x5L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,7,0,0xfL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,7,0,0xbL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,7,0,0xdL))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,0,0x3f7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,0,0x3f7L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETN,3,0,0x3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_RET(___H_join_2d_paths,___IFD(___RETI,4,0,0x3f3L))
,___DEF_LBL_INTRO(___H_resource,0,___REF_FAL,5,0)
,___DEF_LBL_PROC(___H_resource,1,0)
,___DEF_LBL_RET(___H_resource,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_resource,___IFD(___RETN,3,0,0x1L))
,___DEF_LBL_RET(___H_resource,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_resource,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_INTRO(___H_init_2d_opengl_2d_c,0,___REF_FAL,2,init_opengl)
,___DEF_LBL_PROC(___H_init_2d_opengl_2d_c,0,0)
,___DEF_LBL_RET(___H_init_2d_opengl_2d_c,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_init_2d_engine_2d_c,0,___REF_FAL,2,init_engine)
,___DEF_LBL_PROC(___H_init_2d_engine_2d_c,2,0)
,___DEF_LBL_RET(___H_init_2d_engine_2d_c,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_shutdown_2d_engine_2d_c,0,___REF_FAL,2,shutdown_engine)
,___DEF_LBL_PROC(___H_shutdown_2d_engine_2d_c,0,0)
,___DEF_LBL_RET(___H_shutdown_2d_engine_2d_c,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H_run_2d_frame_2d_c,0,___REF_FAL,2,run_frame)
,___DEF_LBL_PROC(___H_run_2d_frame_2d_c,0,0)
,___DEF_LBL_RET(___H_run_2d_frame_2d_c,___IFD(___RETI,0,0,0x3fL))
___END_LBL

___BEGIN_MOD1
___DEF_PRM(0,___G__20_init,1)
___END_MOD1

___BEGIN_MOD2
___END_MOD2

#endif
