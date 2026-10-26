// Farid Allahverdiyev 251ADB0
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// AI = https://gemini.google.com/app/72bd08fb7cdb3400
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>


typedef struct {
    int is_float;
    union {
        long long i;
        double f;
    } v;
} Value;


static const char *file_start = NULL;
static const char *curr_ptr = NULL;
static int global_error_pos = 0; 


Value parse_expr(void);
Value parse_term(void);
Value parse_power(void);
Value parse_primary(void);

void set_error(void) {
    if (global_error_pos == 0) {
        global_error_pos = (int)(curr_ptr - file_start + 1);
    }
}


void skip_whitespace(void) {
    while (*curr_ptr == ' ' || *curr_ptr == '\t') {
        curr_ptr++;
    }
}


Value parse_primary(void) {
    if (global_error_pos) return (Value){0, {0}};
    skip_whitespace();

    if (*curr_ptr == '(') {
        curr_ptr++; 
        Value val = parse_expr();
        skip_whitespace();
        if (*curr_ptr == ')') {
            curr_ptr++; 
            return val;
        } else {
            set_error();
            return (Value){0, {0}};
        }
    }

    if (isdigit((unsigned char)*curr_ptr)) {
        const char *start = curr_ptr;
        int has_dot = 0;
        while (isdigit((unsigned char)*curr_ptr)) {
            curr_ptr++;
        }
        if (*curr_ptr == '.') {
            has_dot = 1;
            curr_ptr++;
            if (!isdigit((unsigned char)*curr_ptr)) {
                
                curr_ptr = start + (curr_ptr - start - 1); 
                set_error();
                return (Value){0, {0}};
            }
            while (isdigit((unsigned char)*curr_ptr)) {
                curr_ptr++;
            }
        }
        
        if (*curr_ptr == 'e' || *curr_ptr == 'E') {
            set_error();
            return (Value){0, {0}};
        }

        char *end;
        if (has_dot) {
            double val = strtod(start, &end);
            return (Value){1, {.f = val}};
        } else {
            long long val = strtoll(start, &end, 10);
            return (Value){0, {.i = val}};
        }
    }

    set_error();
    return (Value){0, {0}};
}


Value parse_power(void) {
    Value left = parse_primary();
    if (global_error_pos) return left;
    skip_whitespace();

    if (*curr_ptr == '*' && *(curr_ptr + 1) == '*') {
        curr_ptr += 2;
        Value right = parse_power(); 
        if (global_error_pos) return left;

        int is_f = left.is_float || right.is_float;
        double base = left.is_float ? left.v.f : (double)left.v.i;
        double exp_val = right.is_float ? right.v.f : (double)right.v.i;

        if (!right.is_float && right.v.i < 0) {
         
            set_error();
            return left;
        }
        if (is_f && base < 0 && floor(exp_val) != exp_val) {
            set_error();
            return left;
        }

        double res = pow(base, exp_val);
        if (isinf(res) || isnan(res)) {
            set_error();
            return left;
        }

        if (!is_f && right.is_float == 0 && right.v.i >= 0) {
            return (Value){0, {.i = (long long)res}};
        } else {
            return (Value){1, {.f = res}};
        }
    }
    return left;
}

Value parse_term(void) {
    Value left = parse_power();
    if (global_error_pos) return left;

    while (1) {
        skip_whitespace();
        char op = *curr_ptr;
        if (op != '*' && op != '/') break;
        curr_ptr++; 

        Value right = parse_power();
        if (global_error_pos) return left;

        int is_f = left.is_float || right.is_float;
        if (op == '*') {
            if (is_f) {
                double l = left.is_float ? left.v.f : (double)left.v.i;
                double r = right.is_float ? right.v.f : (double)right.v.i;
                left = (Value){1, {.f = l * r}};
            } else {
                left = (Value){0, {.i = left.v.i * right.v.i}};
            }
        } else { 
            if (is_f) {
                double l = left.is_float ? left.v.f : (double)left.v.i;
                double r = right.is_float ? right.v.f : (double)right.v.i;
                if (r == 0.0) {
                   
                    set_error();
                    return left;
                }
                left = (Value){1, {.f = l / r}};
            } else {
                if (right.v.i == 0) {
                    set_error();
                    return left;
                }
                left = (Value){0, {.i = left.v.i / right.v.i}};
            }
        }
    }
    return left;
}


Value parse_expr(void) {
    Value left = parse_term();
    if (global_error_pos) return left;

    while (1) {
        skip_whitespace();
        char op = *curr_ptr;
        if (op != '+' && op != '-') break;
        curr_ptr++; // consume operator

        Value right = parse_term();
        if (global_error_pos) return left;

        int is_f = left.is_float || right.is_float;
        if (op == '+') {
            if (is_f) {
                double l = left.is_float ? left.v.f : (double)left.v.i;
                double r = right.is_float ? right.v.f : (double)right.v.i;
                left = (Value){1, {.f = l + r}};
            } else {
                left = (Value){0, {.i = left.v.i + right.v.i}};
            }
        } else {
            if (is_f) {
                double l = left.is_float ? left.v.f : (double)left.v.i;
                double r = right.is_float ? right.v.f : (double)right.v.i;
                left = (Value){1, {.f = l - r}};
            } else {
                left = (Value){0, {.i = left.v.i - right.v.i}};
            }
        }
    }
    return left;
}

void evaluate_line(const char *line_start, const char *line_end, FILE *out) {
    curr_ptr = line_start;
    global_error_pos = 0;

    skip_whitespace();
    if (curr_ptr >= line_end || *curr_ptr == '\r' || *curr_ptr == '\n') {
        return; 
    }

    Value result = parse_expr();
    skip_whitespace();

    if (!global_error_pos && curr_ptr < line_end && *curr_ptr != '\r' && *curr_ptr != '\n' && *curr_ptr != '#') {
        set_error();
    }

    if (global_error_pos) {
        fprintf(out, "ERROR:%d\n", global_error_pos);
    } else {
        if (result.is_float) {
            fprintf(out, "%.15g\n", result.v.f);
        } else {
            fprintf(out, "%lld\n", result.v.i);
        }
    }
}

void process_file(const char *filepath, const char *outpath) {
    FILE *f = fopen(filepath, "rb");
    if (!f) {
        fprintf(stderr, "Error opening input file: %s\n", filepath);
        exit(1);
    }

    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = malloc(len + 1);
    if (!buf) {
        fclose(f);
        fprintf(stderr, "Memory allocation error\n");
        exit(1);
    }
    size_t bytes_read = fread(buf, 1, len, f);
(void)bytes_read;
    buf[len] = '\0';
    fclose(f);

    FILE *out = fopen(outpath, "w");
    if (!out) {
        fprintf(stderr, "Error opening output file: %s\n", outpath);
        free(buf);
        exit(1);
    }

    file_start = buf;
    const char *ptr = buf;
    const char *file_end = buf + len;

    while (ptr < file_end) {
        const char *line_start = ptr;
        const char *line_end = ptr;
        while (line_end < file_end && *line_end != '\n') {
            line_end++;
        }

        const char *p = line_start;
        while (p < line_end && (*p == ' ' || *p == '\t')) p++;

        if (p < line_end && *p == '#') {
        
        } else {

            evaluate_line(line_start, line_end, out);
        }

        ptr = line_end + 1; 
    }

    free(buf);
    fclose(out);
}

void ensure_directory(const char *path) {
    struct st;
    mkdir(path, 0777);
}

int main(int argc, char *argv[]) {
    char *dir_arg = NULL;
    char *out_arg = NULL;
    char *input_arg = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 < argc) dir_arg = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 < argc) out_arg = argv[++i];
        } else if (!input_arg) {
            input_arg = argv[i];
        } else {
            fprintf(stderr, "Unknown or unexpected argument: %s\n", argv[i]);
            return 1;
        }
    }


    const char *student_name = "anna_berzina";
    const char *student_id = "221234";

    if (dir_arg) {
        DIR *d = opendir(dir_arg);
        if (!d) {
            fprintf(stderr, "Cannot open directory: %s\n", dir_arg);
            return 1;
        }

        char outdir[512];
        if (out_arg) {
         snprintf(outdir, sizeof(outdir), "%s", out_arg);
        } else {
            snprintf(outdir, sizeof(outdir), "%s_%s_%s", dir_arg, student_name, student_id);
        }
        ensure_directory(outdir);

        struct dirent *dir;
        while ((dir = readdir(d)) != NULL) {
            char *ext = strrchr(dir->d_name, '.');
            if (ext && strcmp(ext, ".txt") == 0) {
                char inpath[1024], outpath[1024];
                snprintf(inpath, sizeof(inpath), "%s/%s", dir_arg, dir->d_name);
                
                // Extract base name without .txt
                char base_name[256];
                size_t base_len = ext - dir->d_name;
                strncpy(base_name, dir->d_name, base_len);
                base_name[base_len] = '\0';

                snprintf(outpath, sizeof(outpath), "%s/%s_%s_%s.txt", outdir, base_name, student_name, student_id);
                process_file(inpath, outpath);
            }
        }
        closedir(d);
    } else if (input_arg) {
        char outdir[512];
        char *ext = strrchr(input_arg, '.');
        char base_name[256] = "task";
        if (ext) {
            char *last_slash = strrchr(input_arg, '/');
            char *start_name = last_slash ? last_slash + 1 : input_arg;
            size_t len = ext - start_name;
            strncpy(base_name, start_name, len);
            base_name[len] = '\0';
        }

        if (out_arg) {
          snprintf(outdir, sizeof(outdir), "%s", out_arg);
        } else {
            snprintf(outdir, sizeof(outdir), "%s_%s_%s", base_name, student_name, student_id);
        }
        ensure_directory(outdir);

        char outpath[1024];
        snprintf(outpath, sizeof(outpath), "%s/%s_%s_%s.txt", outdir, base_name, student_name, student_id);
        process_file(input_arg, outpath);
    } else {
        fprintf(stderr, "Missing input arguments or directory.\n");
        return 1;
    }

    return 0;
}