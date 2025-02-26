### Задача 13:
  Даны регулярное выражение и слово. Найти подслово максимальной длины. принадлежащее регулярному выражению.
Будем строить НКА без эпсилон переходов для регулярного выражения, а затем для каждого суффикса данного слова найдем длину максимального префикса, принадлежащего регулярному выражению.
### Сложность:
Пусть длина регулярного выражения - n, а длина слова - m. 
  1) НКА без эпсилон переходов построим за O(n^3):
Будем кидать на стэк автоматы соответствующие промежуточным регулярным выражениям, затем доставать и сливать, в зависимости от внешней операции. Итераций будет O(n), сливать автоматы будем в худшем случае за O(n^2), т.к. количество вершин в автомате - O(n).  
  2) Найти длину максимального префикса, принадлежащего регулярному языку можно за O(L*n^2), где L - длина слова:
Инициализируем множество стартовой вершиной автомата. Затем за O(n^2) будем переходить к множеству вершин, достижимых из текущей по соответсвенной букве слова. Если множество содержит терминальные вершины будем обновлять максимум 
  3) Найдем длину максимального подслова запустив поиск максимального префикса для всех суффиксов данного слова за O(m^2 * n^2) 
Итоговая асимптотика - O((n + m^2)*n^2) 

### Тесты: 
Тесты лежат в файле "test.txt"\
Тестирование запускается флагом -t \
Пример: ./a.out -t \
Результаты тестов записываются в файл "output.txt", на экран выводится количество пройденных тестов
