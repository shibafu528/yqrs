#ifndef yq_h
#define yq_h

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>


typedef enum YQ_ParseStatus {
  YQ_ParseStatus_Success = 0,
  YQ_ParseStatus_InvalidQuery,
  YQ_ParseStatus_UnexpectedToken = 1000,
  YQ_ParseStatus_UnexpectedEOF,
  YQ_ParseStatus_UnparseableNumber,
  YQ_ParseStatus_UnterminatedList,
  YQ_ParseStatus_LexerStringIsNotClosed = 2000,
} YQ_ParseStatus;

typedef struct YQ_Query YQ_Query;

typedef struct YQ_Source YQ_Source;

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

enum YQ_ParseStatus yq_v1_parse(const char *query, struct YQ_Query **out);

void yq_v1_query_free(struct YQ_Query *query);

const struct YQ_Source *yq_v1_query_get_source(const struct YQ_Query *query, size_t index);

size_t yq_v1_query_get_sources_size(const struct YQ_Query *query);

size_t yq_v1_source_get_argument(const struct YQ_Source *source, const char **out);

size_t yq_v1_source_get_class(const struct YQ_Source *source, const char **out);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif /* yq_h */
