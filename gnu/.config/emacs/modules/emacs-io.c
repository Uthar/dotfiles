#include <emacs-module.h>
#include <stdio.h>
#include <stdlib.h>

// Expose to elisp I/O functions described in The GNU C Library Reference Manual

int plugin_is_GPL_compatible;

char *linebuf;

emacs_value
emacs_io_fopen (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  emacs_value pathname = args[0];
  emacs_value mode = args[1];
  int pathnamesize = 1024;
  int modesize = 8;
  char *c_pathname = malloc(pathnamesize * sizeof(char));
  char *c_mode = malloc(modesize * sizeof(char));
  env->copy_string_contents(env, pathname, c_pathname, &pathnamesize);
  env->copy_string_contents(env, mode, c_mode, &modesize);
  FILE *stream = fopen(c_pathname, c_mode);
  free(c_pathname);
  free(c_mode);
  return env->make_user_ptr(env, NULL, stream);
}

emacs_value
emacs_io_fclose (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  emacs_value stream = args[0];
  FILE *c_stream = env->get_user_ptr(env, stream);
  fclose(c_stream);
  return env->intern(env, "t");
}

/* emacs_value */
/* emacs_io_fwrite (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) */
/* { */
/*   emacs_value data = args[0]; */
/*   emacs_value size = args[1]; */
/*   emacs_value count = args[2]; */
/*   emacs_value stream = args[3]; */
/*   char c_size */
/*   char *c_data = malloc */
/* } */

emacs_value
emacs_io_getline (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  emacs_value stream = args[0];
  FILE *c_stream = env->get_user_ptr(env, stream);
  size_t n = 1024;
  ssize_t nbytes = getline(&linebuf, &n, c_stream);
  if (nbytes <= 0)
    return env->funcall(env, env->intern(env, "string"), 0, NULL);
  emacs_value line = env->make_string(env, linebuf, nbytes);
  realloc(linebuf, 1024 * sizeof(char));
  return line;
}


int
emacs_module_init (struct emacs_runtime *runtime)
{
  if (runtime->size < sizeof (*runtime))
    return 1;
  emacs_env *env = runtime->get_environment (runtime);
  if (env->size < sizeof (*env))
    return 2;
  linebuf = malloc(1024 * sizeof(char));
  
  emacs_value fn_fopen = env->make_function(env, 2, 2, &emacs_io_fopen, NULL, NULL);
  emacs_value sym_fopen = env->intern(env, "fopen");
  emacs_value args_fopen[] = {sym_fopen, fn_fopen};
  env->funcall(env, env->intern(env, "defalias"), 2, args_fopen);

  emacs_value fn_fclose = env->make_function(env, 1, 1, &emacs_io_fclose, NULL, NULL);
  emacs_value sym_fclose = env->intern(env, "fclose");
  emacs_value args_fclose[] = {sym_fclose, fn_fclose};
  env->funcall(env, env->intern(env, "defalias"), 2, args_fclose);

  emacs_value fn_getline = env->make_function(env, 1, 1, &emacs_io_getline, NULL, NULL);
  emacs_value sym_getline = env->intern(env, "getline");
  emacs_value args_getline[] = {sym_getline, fn_getline};
  env->funcall(env, env->intern(env, "defalias"), 2, args_getline);
  return 0;
}
