#ifndef yq_h
#define yq_h

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>


typedef enum YQ_EvalError {
  YQ_EvalError_Success = 0,
  YQ_EvalError_VoidFunction,
  YQ_EvalError_InvalidFunction,
  YQ_EvalError_VoidVariable,
} YQ_EvalError;

typedef enum YQ_ParseStatus {
  YQ_ParseStatus_Success = 0,
  YQ_ParseStatus_InvalidQuery,
  YQ_ParseStatus_UnexpectedToken = 1000,
  YQ_ParseStatus_UnexpectedEOF,
  YQ_ParseStatus_UnparseableNumber,
  YQ_ParseStatus_UnterminatedList,
  YQ_ParseStatus_LexerStringIsNotClosed = 2000,
} YQ_ParseStatus;

typedef struct YQ_Context YQ_Context;

typedef struct YQ_Expression YQ_Expression;

typedef struct YQ_Query YQ_Query;

typedef struct YQ_Source YQ_Source;

typedef struct YQ_StringRef {
  const char *ptr;
  size_t len;
} YQ_StringRef;

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

struct YQ_Expression *yq_v1_context_eval(struct YQ_Context *context,
                                         const struct YQ_Expression *expr);

void yq_v1_context_free(struct YQ_Context *context);

enum YQ_EvalError yq_v1_context_get_last_error(struct YQ_Context *context);

struct YQ_Context *yq_v1_context_new(void);

void yq_v1_expression_free(struct YQ_Expression *expr);

bool yq_v1_expression_is_nil(const struct YQ_Expression *expr);

enum YQ_ParseStatus yq_v1_parse(const char *query, struct YQ_Query **out);

void yq_v1_query_free(struct YQ_Query *query);

const struct YQ_Expression *yq_v1_query_get_expression(const struct YQ_Query *query);

const struct YQ_Source *yq_v1_query_get_source(const struct YQ_Query *query, size_t index);

size_t yq_v1_query_get_sources_size(const struct YQ_Query *query);

struct YQ_StringRef yq_v1_source_get_argument(const struct YQ_Source *source);

struct YQ_StringRef yq_v1_source_get_class(const struct YQ_Source *source);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif /* yq_h */
