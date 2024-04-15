# Реализация intermediate language (IL)

Базовый синтаксис взят из статьи [The next 700 program transformers](https://link.springer.com/chapter/10.1007/978-3-030-98869-2_7):

$$
\begin{alignat*}{3}
 &prog &&::= e_0\ \mathbf{where}\  h_1 = e_1\ \dots \ h_n = e_n&\\
 &e &&::=  \ x&\\
        &&\mid & \ c\ e_1 \dots e_n&\\
        &&\mid & \ \lambda x.e&\\
        &&\mid & \ f&\\
        &&\mid & \ e_0 \ e_1&\\
        &&\mid & \ \mathbf{case} \ e_0 \ \mathbf{of}\ p_1\Rightarrow e_1\dots\ p_n\Rightarrow e_n&\\
        &&\mid & \ \mathbf{let} \ x = e_0 \ \mathbf{in}\ e_1&\\
&h &&::= \ f \ x_1\dots x_n&\\
&p && ::= \ c \ x_1\dots x_n&
\end{alignat*}
$$
