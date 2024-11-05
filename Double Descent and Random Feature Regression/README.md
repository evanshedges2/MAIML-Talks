# Double Descent and Random Feature Regression

Presentation given on 04 November, 2024. 

Abstract: This talk will explore the double descent phenomenon by means of random feature regression. 

First observed in 2018 by Belkin et. al (link below), double descent is a strange and poorly understood phenomenon that counters typical statistical intuition. According to classical statistics, as the number of model parameters (p) increases, typically you will find an increase in model performance on out of sample data, followed by a sharp decrease that only gets worse as you add more parameters. Recent empirical work has shown that this not the case. Instead, the generalizability of a particular model generally exhibits a “double descent” phenomenon, first following classical statistics intuition until the interpolation threshold of p=n, but counterintuitively once you enter the overparameterized regime (p>n), the model beings to perform better out of sample once again. 

Citations: 
* Belkin 2018: https://arxiv.org/abs/1812.11118 
* OpenAI 2019: https://arxiv.org/abs/1912.02292 

Other sources: 
* https://www.greaterwrong.com/posts/FRv7ryoqtvSuqBxuT/understanding-deep-double-descent
* https://arxiv.org/abs/1908.05355 
