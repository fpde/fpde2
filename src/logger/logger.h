#ifdef logdebug

#define __A(msg) #msg
#define __B(msg) __A(msg)

#define log(lvl,msg)  log(lvl,"("//__FILE__ // ":" // __B(__LINE__) // ") " // msg)

#define loge(msg)     loge("("//__FILE__ // ":" // __B(__LINE__) // ") " // msg)
#define logw(msg)     logw("("//__FILE__ // ":" // __B(__LINE__) // ") " // msg)
#define logi(msg)     logi("("//__FILE__ // ":" // __B(__LINE__) // ") " // msg)
#define logd(msg)     logd("("//__FILE__ // ":" // __B(__LINE__) // ") " // msg)

#endif
