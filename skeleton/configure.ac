AC_PREREQ(2.59)
AC_INIT(<% @var name %>, 1.0, <% @var email %>)

AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP)
AC_SUBST(<% @var name %>_ASDF_DIR)

# Checks for programs.
AC_ARG_WITH(lisp,    [  --with-lisp=IMPL        use the specified lisp (sbcl, clisp, ccl or ecl)], LISP=$withval, LISP="any")
AC_ARG_WITH(ecl,     [  --with-ecl=PATH         specify location of ecl], ECL_PATH=$withval, ECL_PATH="")
AC_ARG_WITH(sbcl,    [  --with-sbcl=PATH        specify location of sbcl], SBCL_PATH=$withval, SBCL_PATH="")


<% @var name %>_ASDF_DIR="`pwd`"

if test -x "$ECL_PATH"; then
   ECL=$ECL_PATH
   AC_MSG_CHECKING([for ecl])
   AC_MSG_RESULT($ECL)
else
   AC_PATH_PROG([ECL], ecl,"")
fi

if test -x "$SBCL_PATH"; then
   SBCL=$SBCL_PATH
   AC_MSG_CHECKING([for sbcl])
   AC_MSG_RESULT($SBCL)
else
   AC_PATH_PROG([SBCL], sbcl,"")
fi

if test "x$LISP" = "xany"; then
   if test "$SBCL"; then
      LISP=sbcl
   elif test "$ECL"; then
      LISP=ecl
   fi
fi

if test "x$LISP" = "xecl"; then
   LISP_PROGRAM=$ECL
elif test "x$LISP" = "xsbcl"; then
   LISP_PROGRAM=$SBCL
fi

if test "x$LISP_PROGRAM" = "x"; then
   AC_MSG_ERROR([*** No lisp is available.])
fi

AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_OUTPUT(Makefile)
