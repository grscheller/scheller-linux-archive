# What is a tensor?

As a college Sophmore, back in the early 1980's, I was taking a
mathematical methods of physics course and I was finally going
to learn what a tensor was!!!  Going into the course, I thought
tensors were some sort of multidimensional generalization of vectors.
Vectors being one dimensional tensors and scalars zero dimensional tensors.
Sort of made sense, since things like the metric tensor `gᵢⱼ` were
always shown as matrices.  I figured a third ranked tensor was just
some sort of `3×3×3` array of numbers.  WRONG!  

The course covered Cartesian tensors.  As for their definion, a tensor
was just any entity that transfored like
```
   Μᵢₖ…ᵣ = UᵢⱼUₖₗ ⋅⋅⋅ Uᵣₛ Μⱼₗ…ₛ
```
where the `Uᵢⱼ` are rotation matrices. (Repeated indices are summed
over - Einstein summation convention.)

That was it. We manipulated tensors, proved alot of things about
orthogonal curvilinear coordinates, and did some physics problems
involving tensors.  We never again addressed what exactly a
tensor was.

My goal in this project is to help people obtain real intuition about
what exactly a tensor is. 
