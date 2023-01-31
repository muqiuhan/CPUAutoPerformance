#lang racket

(define ->>
  (λ args
    (let ((v (car args))
          (functions (cdr args)))
      (foldl (λ (f v) (f v)) v functions))))

(define (cpu-info)
  (->>
   (with-output-to-string
     (λ ()
       (system "cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor")))
   (λ (cpu-info-str) (string-split cpu-info-str "\n"))))

(define (set-cpu-mode mode)
  (letrec ([set-mode (λ (mode)
                       (system (format "echo \"~a\" | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor" mode)))])
    (cond
      ((eq? mode 'performance) (set-mode "performance"))
      ((eq? mode 'powersave) (set-mode "powersave")))))

(define (battery-charging?)
  (->>
   (with-output-to-string
     (λ ()
       (system "cat /sys/class/power_supply/BAT0/status")))
   (λ (battery-charging?) (not (string=? battery-charging? "Discharging\n")))))

(define (battery-capacity)
  (->>
   (with-output-to-string
     (λ ()
       (system "cat /sys/class/power_supply/BAT0/capacity")))
   (λ (battery-capacity) (string->number battery-capacity))))

(define CPU_PERFORMANCE_STRATEGY
  (λ ()
    (if (battery-charging?)
        (set-cpu-mode 'performance)
        (set-cpu-mode 'powersave))))

(define CHECK_INTERVAL 60)

(letrec ([running (lambda ()
                    (CPU_PERFORMANCE_STRATEGY)
                    (sleep 60)
                    (running))])
  (running))
