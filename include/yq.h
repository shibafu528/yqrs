#ifndef yq_h
#define yq_h

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>


typedef enum YQ_ExprType {
  YQ_ExprType_Nil,
  YQ_ExprType_Symbol,
  YQ_ExprType_String,
  YQ_ExprType_Integer,
  YQ_ExprType_Float,
  YQ_ExprType_Reference,
  YQ_ExprType_Cons,
} YQ_ExprType;

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

typedef struct YQ_Expression *(*YQ_Function)(struct YQ_Context *context, void *user_data, const char *symbol, const struct YQ_Expression *cdr, bool *error);

typedef uint32_t YQ_CompatFlags;
/**
 * 1つの引数で `equals` を評価する際、その引数が `t` であるかを返すようにする。
 * この互換性フラグを使用しない場合、そのような状況では無条件で `t` を返す。
 */
#define YQ_CompatFlags_RETURN_COMPARE_TO_T_IN_SINGLE_ARGUMENT_EQUALS (uint32_t)1
/**
 * 引数なしで `or` を評価する際、`nil` の代わりに `t` を返すようにする。
 */
#define YQ_CompatFlags_RETURN_TRUE_IN_NO_ARGUMENTS_OR (uint32_t)2
/**
 * `and` および `or` の引数を評価する際、厳密に `t` である場合のみそれが真であると判断する。
 * この互換性フラグを使用しない場合、`nil` 以外であればすべて真であると判断する。
 */
#define YQ_CompatFlags_EXPLICIT_COMPARE_TO_T_IN_AND_OR (uint32_t)4
/**
 * `nil` の引数を評価する際、厳密に `t` である場合のみそれが真であると判断する。
 * この互換性フラグを使用しない場合、`nil` 以外であればすべて真であると判断する。
 */
#define YQ_CompatFlags_EXPLICIT_COMPARE_TO_T_IN_NOT (uint32_t)8

typedef struct YQ_Expression *(*YQ_MethodDispatcherCallback)(const char *symbol, const struct YQ_Expression *receiver, const struct YQ_Expression *cddr, bool *error);

typedef struct YQ_Expression *(*YQ_VariableProviderCallback)(const char *symbol);

typedef struct YQ_StringRef {
  const char *ptr;
  size_t len;
} YQ_StringRef;

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

struct YQ_Expression *yq_v1_context_eval(struct YQ_Context *context,
                                         const struct YQ_Expression *expr,
                                         bool *error);

void yq_v1_context_free(struct YQ_Context *context);

struct YQ_Context *yq_v1_context_new(void);

void yq_v1_context_register_function(struct YQ_Context *context,
                                     const char *symbol,
                                     YQ_Function function,
                                     void *user_data);

void yq_v1_context_set_compat_flags(struct YQ_Context *context, YQ_CompatFlags flags);

void yq_v1_context_set_method_dispatcher(struct YQ_Context *context,
                                         YQ_MethodDispatcherCallback callback);

void yq_v1_context_set_variable_provider(struct YQ_Context *context,
                                         YQ_VariableProviderCallback callback);

void yq_v1_expression_free(struct YQ_Expression *expr);

const struct YQ_Expression *yq_v1_expression_get_car(const struct YQ_Expression *expr);

const struct YQ_Expression *yq_v1_expression_get_cdr(const struct YQ_Expression *expr);

double yq_v1_expression_get_float(const struct YQ_Expression *expr);

int64_t yq_v1_expression_get_integer(const struct YQ_Expression *expr);

uint64_t yq_v1_expression_get_reference(const struct YQ_Expression *expr);

struct YQ_StringRef yq_v1_expression_get_string(const struct YQ_Expression *expr);

enum YQ_ExprType yq_v1_expression_get_type(const struct YQ_Expression *expr);

bool yq_v1_expression_is_nil(const struct YQ_Expression *expr);

struct YQ_Expression *yq_v1_expression_new_cons(struct YQ_Expression *car,
                                                struct YQ_Expression *cdr);

struct YQ_Expression *yq_v1_expression_new_float(double value);

struct YQ_Expression *yq_v1_expression_new_integer(int64_t value);

struct YQ_Expression *yq_v1_expression_new_nil(void);

struct YQ_Expression *yq_v1_expression_new_reference(uint64_t ref);

struct YQ_Expression *yq_v1_expression_new_string(const char *string);

struct YQ_Expression *yq_v1_expression_new_symbol(const char *symbol);

struct YQ_Expression *yq_v1_expression_new_t(void);

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
