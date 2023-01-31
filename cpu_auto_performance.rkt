;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIT LICENSE                                                                    ;;
;;                                                                                ;;
;; Copyright (c) 2022 Muqiu Han                                                   ;;
;;                                                                                ;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy   ;;
;; of this software and associated documentation files (the "Software"), to deal  ;;
;; in the Software without restriction, including without limitation the rights   ;;
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      ;;
;; copies of the Software, and to permit persons to whom the Software is          ;;
;; furnished to do so, subject to the following conditions:                       ;;
;;                                                                                ;;
;; The above copyright notice and this permission notice shall be included in all ;;
;; copies or substantial portions of the Software.                                ;;
;;                                                                                ;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     ;;
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       ;;
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    ;;
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         ;;
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  ;;
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  ;;
;; SOFTWARE.                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(letrec ([running (λ ()
                    (CPU_PERFORMANCE_STRATEGY)
                    (sleep 60)
                    (running))])
  (running))
