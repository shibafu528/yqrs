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

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

enum YQ_ParseStatus yq_v1_parse(const char *query, struct YQ_Query **out);

void yq_v1_query_free(struct YQ_Query *query);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif /* yq_h */
