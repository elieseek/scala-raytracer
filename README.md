![Scala CI](https://github.com/elieseek/scala-raytracer/workflows/Scala%20CI/badge.svg)
# scala-raytracer
Learning scala by trying to create a raytracer using Peter Shirley's [series of guides on the topic](https://raytracing.github.io/books/RayTracingInOneWeekend.html#overview) written in C++
# Result
![with lighting](https://github.com/elieseek/scala-raytracer/blob/master/result/lighting%201920.png)
![final product](https://github.com/elieseek/scala-raytracer/blob/master/result/final.png)

# Changes to guide
- Option with match expressions was used in place of pointers
- Case classes used instead of shared pointers for materials and spheres
- Multithreaded (without changing gamma)

# Future work
- Improve efficiency of importance sampling boxes
- Improve multiple importance sampling
- Invent time machine to go back and choose a faster language before starting this
