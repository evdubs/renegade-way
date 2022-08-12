#lang racket/base

; cmd-line moved up here to be loaded before other bits that need params set by cmd-line
(require "cmd-line.rkt")

(require "gui/chart.rkt"
         "gui/option-strategy-frame.rkt"
         "gui/position-order-manager.rkt"
         "gui/analysis.rkt"
         "logging.rkt")

(start-logging)

(show-chart)

(show-option-strategy)

(show-position-order-manager)

(show-analysis)
