# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

print("Hello World")

import random
import adaptive
import holoviews
import bokeh
import ipywidgets
import adaptive.notebook_integration
#from adaptive.learner.base_learner import BaseLearner
#from adaptive.notebook_integration import ensure_holoviews
#from adaptive.utils import cache_latest
adaptive.notebook_extension()

offset = random.uniform(-0.5, 0.5)

def f(x, offset=offset, wait=True):
    from time import sleep
    from random import random

    a = 0.01
    if wait:
        sleep(random() / 10)
    return x + a**2 / (a**2 + (x - offset)**2)

learner = adaptive.Learner1D(f, bounds=(-1, 1))

runner = adaptive.Runner(learner, goal=lambda l: l.loss() < 0.01)

runner.live_info()

runner.live_plot(update_interval=0.1)
