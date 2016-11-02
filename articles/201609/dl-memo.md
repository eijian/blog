# Deep Learningに関するメモ１

## 全結合層におけるwijの変化量導出

変化量は次の通り

\[
\frac{\partial E_n}{\partial w_{ji}} = \frac{\partial E_n}{\partial a_j}
                                       \frac{\partial a_j}{\partial w_{ji}}
\]

第二項について、

\[
a_j = \sum_{k} w_{jk} x_k + b_j
\]

だから、

\[
\frac{\partial a_j}{\partial w_{ji}} = x_i
\]

である。また、

\[
\frac{\partial E_n}{\partial a_j} = \delta_j
\]

とすれば、

\[
\frac{\partial E_n}{\partial w_{ji}} = \delta_j x_i
\]

と書ける。

### 出力層の場合

\[
\delta_j = \sum_k \frac{\partial E_n}{\partial y_{n,k}} \frac{\partial y_{n,k}}{\partial a_j}
\]

と書ける。活性関数$h$をsoftmax関数とし、損失関数を交差エントロピー誤差(kクラス分類)と
した場合、

\[
softmax: h(a) = \frac{e^{a_i}}{\sum_k e^{a_k}}
\]
\[
E_n = -\sum_k t_{n,k} \ln y_{n,k}
\]

である。softmax関数の偏微分は

\[
\frac{\partial y_{n,k}}{\partial a_j} = y_{n,k} (1-y_{n,j}), -y_{n,j}
\]

$\delta_j$を計算すると、

\[
\delta_j = \frac{\partial E_n}{\partial a_j}
\]
\[
= -\sum_k t_{n,k} \frac{\ln y_{n,k}}{\partial y_{n,k}}
  (y_{n,k}(1-y_{n,j}), -y_{n,k} y_{n,j}))
\]
\[
= -\sum_k t_{n,k} \frac{1}{y_{n,k}} (y_{n,k}(1-y_{n,j}), -y_{n,k} y_{n,j})
\]
\[
= -\sum_k t_{n,k} (1-y_{n,j}, -y_{n,j})
\]

展開すると、$j$番目だけが余分になり、

\[
= -t_{n,1} \cdot -y_{n,j} + (-t_{n,j}) + (-t_{n,j} \cdot -y_{n,j})+ ...
\]
\[
= \sum_k t_{n,k} y_{n,j} - t_{n,j}
\]
\[
= y_{n,j} - t_{n,j}
\]
