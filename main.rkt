#lang racket/base

; cmd-line moved up here to be loaded before other bits that need params set by cmd-line
(require "cmd-line.rkt")

(require "chart.rkt"
         "logging.rkt"
         "option-strategy-frame.rkt"
         "position-order-manager.rkt"
         "analysis.rkt")

(start-logging)

(show-chart)

(show-option-strategy)

(show-position-order-manager)

(show-analysis)
