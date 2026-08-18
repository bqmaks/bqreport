# SESSION_HANDOFF

Состояние на 2026-08-18.

Контракт, инварианты и соглашения — в `AGENTS.md`. Здесь только состояние.

## Состояние git

- Ветка: `agent/stabilize-comparison-api`.
- HEAD: `a8de6e9 Stabilize comparison analysis workflows`.
- После HEAD есть 34 изменённых или новых пути с текущей реализацией семейств
  сравнений. Они намеренные, ещё не закоммичены и не должны быть отброшены или
  перезаписаны. Коммит и push делать только по новой явной просьбе автора.

## Сделано

- `R/conditions.R` — `bq_abort()`, типизированные условия.
- `R/bq-data.R` — `as_bq_data()`, конструкторы `bq_data` и реестра.
- `R/registry.R` — согласование реестра со столбцами и монотонная выдача
  идентификаторов через внутренний `next_var_number`; удалённый `var_id`
  никогда не переиспользуется.
- `R/dplyr-methods.R` — сохранение метаданных через dplyr и base replacement;
  `$<-`, `[[<-` и `[<-` выдают ID новым столбцам и сбрасывают зависимые от
  значений метаданные переписанных. `group_by()`, `rowwise()` и
  `tibble::add_column()` отвергаются явно.
- `R/variables.R` — `variables()`, чтение реестра.
- `R/variable-levels.R` — `variable_levels()`, чтение плоского реестра уровней.
- `R/print.R` — заголовок со сводкой по ролям.
- `R/infer-type.R` — `infer_type()`, вывод аналитического типа из вектора.
  Внутренний `infer_type_metadata()` одновременно возвращает event, его
  источник и объявленный порядок ordinal levels; `max_levels` проверяется как
  положительный целый скаляр.
- `R/infer-types.R` — `infer_types()`, применение вывода к неразмеченным
  столбцам без перезаписи уже принятого решения.
- `R/type-constructors.R` — `continuous()`, `count()`, `binary(event)`,
  `ordinal(levels)` и `nominal(reference)`, декларативные спецификации класса
  `bq_type`.
- `R/set-type.R` — `set_type()`, запись явной спецификации одного столбца в
  плоские реестры с проверкой event, reference, ordinal levels и внутренней
  согласованности specification.
- `R/set-role.R` — `set_role()`, установка одной из ролей `outcome`,
  `predictor`, `group`, `id` для одного или нескольких столбцов, и удобная
  обёртки `set_outcome()` и `set_predictor()`.
- `R/set-format.R` — `set_unit()` и `set_rounding()`, запись единиц измерения и
  политики представления (`decimal` / `significant` + `digits`) без изменения
  исходных чисел.
- `R/set-summary-format.R` — `set_summary_format()` и `summary_formats()`.
  Именованный или полностью неименованный character-вектор шаблонов вида
  `{mean} ({sd})` хранится в отдельном плоском реестре `var_id`,
  `format_name`, `template`, `position`. Частично именованные, повторяющиеся
  имена, пустые шаблоны и несогласованные фигурные скобки отвергаются.
  Форматы следуют за стабильным `var_id`, сохраняются при изменении значений
  как metadata намерения и удаляются вместе со столбцом.
- `R/apply-dictionary.R` — `apply_dictionary()`, применение основного плоского
  словаря по имени и отдельного ordinal level dictionary с приоритетом над
  inferred/default и защитой explicit.
- `R/selectors.R` — внутренний `resolve_variables()`, немедленно связывающий
  tidyselect-выбор со стабильными `var_id`; именованные aliases не заменяют
  каноническое имя реестра. На него переведены `set_type()` и `set_role()` с
  его обёртками, а также `infer_types()`.
- `R/plan-summary.R` — `plan_summary()`, конструктор только дизайна: group,
  strata и оси raw Overall. Анализируемые переменные и вычисления в нём не
  задаются; их последовательно добавляет `add_statistic()`. Поэтому только что
  созданный план является допустимым промежуточным объектом с пустым
  `variables`, а preflight возвращает для него blocking
  `missing_summary_variable`. Компактный print показывает дизайн,
  зарегистрированные статистики и назначения, но не вложенные данные и
  функции. План также
  содержит плоские реестры `display_rules` (`rule_id`, `kind`, `max_n`,
  `display_statistics`) и `display_rule_assignments` (`rule_id`, `var_id`) со
  счётчиком `next_display_rule_number`.
- `R/continuous-statistic.R` — `continuous_statistic()`, декларация custom raw
  функции с one-row data-frame prototype, пользовательской missing policy и
  шкалой каждого компонента: `variable`, `count` или `dimensionless`. Скалярный
  `scale` применяется ко всем компонентам, разные шкалы задаются именованным
  вектором; count требует integer storage. Specification также содержит
  именованные `component_rounding` и `component_digits`.
- `R/continuous-descriptives.R` — `continuous_descriptives()`, встроенная
  базовая raw specification с компонентами `mean`, `sd`, `median`, `q1`,
  `q3`, `min`, `max`. Пропуски исключаются системно, квартиль использует
  `quantile(type = 7)`, пустая/all-missing ячейка возвращает NA для всех
  компонентов, а выборка из одного наблюдения — NA для `sd`. Все компоненты
  имеют variable scale и наследуют формат переменной.
- `R/continuous-descriptives-extended.R` —
  `continuous_descriptives_extended()`, отдельная встроенная specification,
  добавляющая к базовому набору `iqr`, `mad`, скорректированный type-2
  `skewness` и `excess_kurtosis`. Моменты вычисляет Suggested-пакет
  `datawizard`; его отсутствие даёт явную `bq_error_missing_dependency` без
  fallback. Чтобы исключить внутренний fallback datawizard с type 2 на type 1,
  skewness вызывается только при n >= 3, kurtosis при n >= 4; нулевой разброс
  даёт NA. IQR согласован с квартилями type 7, MAD использует стандартный
  `stats::mad()`. Два dimensionless-компонента имеют default decimal/2.
- `R/set-component-rounding.R` — `set_component_rounding()`, явная decimal или
  significant policy для variable/dimensionless компонентов. Variable без
  override наследует политику переменной; count всегда остаётся целым и setter
  для него запрещён.
- `R/add-statistic.R` — публичный `add_statistic(plan, variables, statistic =
  continuous_descriptives())`. Tidyselect-выбор одновременно добавляет summary-
  переменные в план в порядке первого выбора и назначает им specification, её
  компоненты (включая `scale`) и исполняемую функцию. Разные стадии pipe могут
  назначать basic, extended или custom specification разным подмножествам.
  Design axes выбирать запрещено. Одинаковые human-readable имена statistics
  допустимы для непересекающихся переменных, поскольку ссылки используют
  `statistic_id`; повтор того же имени для той же переменной отвергается.
- `R/preflight.R` — generic `preflight()` и метод summary-плана; повреждённая
  структура плана отвергается, а аналитические проблемы возвращаются плоской
  таблицей diagnostics с адресацией по `var_id`, `statistic_id`, `component`,
  `rule_id` и `cell_id`.
  Помимо типов, statistic и format policy, проверяются пропуски на осях
  дизайна и пустые leaf-ячейки; последние две проверки дают warning и не
  блокируют выполнение. `enumerate_values` пока совместим только с continuous;
  несовместимость возвращается как блокирующая `incompatible_display_rule`,
  но не дублируется для missing/unknown type. Результат preflight содержит
  скомпилированные `cells`, `cell_axes` и `cell_rows`. Dimensionless component
  без явной политики даёт блокирующую `missing_component_rounding`. Summary
  format registry проверяется структурно; placeholders сопоставляются с
  компонентами statistics, назначенных той же переменной. Отсутствующее имя
  даёт blocking `unknown_summary_component`, одно имя из нескольких statistics
  — `ambiguous_summary_component`; повтор placeholder внутри одного шаблона не
  дублирует diagnostic, а при полном отсутствии statistic остаётся только
  первичная `missing_statistic`.
- `R/compile-cells.R` — внутренний `compile_summary_cells()`, нормализующий
  leaf и raw Overall в три плоских реестра: `cells`, `cell_axes`, `cell_rows`.
  Factor использует объявленные levels, ordinal — плоский level registry,
  включая пустые комбинации; NA остаётся явной ячейкой; multiple strata
  сворачиваются общей осью Overall.
- `R/enumerate-values.R` — `enumerate_values(max_n = 2L,
  display_statistics = FALSE)`, декларативное display rule для перечисления
  исходных непропущенных значений в малых summary-ячейках. При
  `display_statistics = TRUE` представление одновременно сохраняет статистики;
  набор фактически вычисляемых статистик правило никогда не меняет.
- `R/add-display-rule.R` — `add_display_rule()`, регистрация display rule и
  его назначений summary-переменным по стабильным `var_id`; один specification
  можно назначить нескольким переменным, но у переменной может быть только одно
  правило.
- `R/run-analysis.R` — generic `run_analysis()` и summary-метод. Перед
  вычислением выполняется preflight-gate; blocking diagnostics становятся
  `bq_error_preflight` с приложенным preflight-объектом. Raw continuous engine
  передаёт custom-функции исходный вектор с NA и в исходном порядке для каждой
  ячейки, включая empty и Overall. Результат `bq_result_summary` хранит plan,
  diagnostics, cell registries, системные `sample_sizes` (`n`, `n_missing`) и
  длинный `estimates` без округления. Runtime error и изменение one-row схемы
  останавливают расчёт как `bq_error_statistic_runtime` или
  `bq_error_statistic_schema`, с ID ячейки, переменной и статистики.
- `R/model-frame.R` — единое приведение continuous storage к plain double.
  Числовые character/factor значения допустимы, неоднозначные, бесконечные и
  неатомарные блокируются в preflight. Исходный `bq_data` не изменяется.
- `R/prepare-presentation.R` — generic `prepare_presentation()` и
  summary-метод. Вычисления не меняются: создаются `display_cells`
  (`cell_id`, `var_id`, `status`, `show_statistics`, `show_values`, `rule_id`)
  и raw `display_values` (`cell_id`, `var_id`, `position`, `value`). Status
  различает `observed`, `all_missing` и `empty`; для observed-ячеек с
  `n <= max_n` флаги позволяют показывать перечисление вместо статистик или
  одновременно с ними. NA исключаются, порядок и повторы, включая raw Overall,
  сохраняются; числа ещё не форматируются.
- `R/format-presentation.R` — generic `format_presentation()` и summary-метод.
  Формирует плоские `formatted_estimates`, `formatted_values`, склеенные
  `enumerations` и `status_text`, не меняя raw result/presentation. Variable
  использует component override, затем policy переменной и только затем явно
  переданный fallback; count печатается целым, dimensionless использует
  component policy. Поддерживаются произвольный одиночный разделительный
  `decimal_mark`, автоматический `; ` при десятичной запятой, пользовательский
  separator, missing/empty tokens и `trim_trailing_zeros`; significant format
  сохраняет заявленные цифры и scientific notation. Unit остаётся отдельной
  metadata и не повторяется после каждого значения.
- `R/compose-table.R` — generic `compose_table()` и summary-метод. Без
  численных вычислений и renderer-specific зависимостей раскладывает уже
  отформатированное представление в плоские реестры `rows`, `columns`,
  `column_axes`, `cell_displays` и `body`. Порядок строк следует переменным,
  статистикам и компонентам плана; enumeration может заменять statistic-строки
  или следовать после них. Для `all_missing` и `empty` body не дублируется:
  приоритетный текст остаётся в `cell_displays$status_text`. Если у переменной
  есть summary formats, component rows заменяются строками
  `row_kind = "summary_format"`: пользовательское имя хранится отдельно в
  `row_label`, исходный шаблон — в `template`, а body собирается подстановкой
  уже отформатированных estimates. Неупомянутые компоненты остаются в result,
  но отдельно не выводятся; unnamed format имеет `row_label = NA`, совместный
  режим с enumeration и полная замена statistics сохраняют прежнюю семантику.
- `demo/continuous-summary.R` — запускаемый сквозной пример continuous summary
  от `as_bq_data()` и словаря метаданных до renderer-neutral `bq_table`.
  Показывает tidyselect, raw Overall по group/strata, basic descriptives,
  одновременное перечисление и статистики в малых ячейках, decimal comma,
  удаление висящих нулей, а также отдельные basic, extended и custom
  specification через последовательные `add_statistic()`.
  Зарегистрирован в `demo/00Index`.
- К `bq_data` добавлен плоский реестр уровней `var_id`, `value`, `position`;
  dplyr сохраняет его, удаляет строки исчезнувших переменных и очищает уровни
  переписанного через `mutate()` столбца.
- К `bq_data` добавлен плоский реестр summary formats `var_id`, `format_name`,
  `template`, `position`; все пути dplyr/base reconstruction сохраняют его и
  фильтруют по существующим `var_id`, а `as_tibble()` снимает вместе с другими
  metadata.

Реестр сейчас: `var_id`, `name`, `label`, `role`, `type`, `event`,
`event_source`, `reference`, `type_source`, `unit`, `rounding`, `digits`.
Экспортируются конструктор данных и accessor, конструкторы типов, явные
сеттеры ролей и типов, `infer_type()` и `infer_types()`.

1013 тестов, `R CMD check --no-manual` — Status: OK. Коммиты `d4650bc`, `d0243f4`,
`6c9e384` запушены в `origin/main`; последующие завершённые срезы зафиксированы
локальными коммитами `3ab2bcc` и `4bd8669`.

## Сравнение групп

- `t_test()` — Student/Welch, двусторонняя, equivalence, noninferiority и
  superiority гипотезы; Cohen's d и Hedges' g; аналитический и
  перестановочный вывод; ordinary bootstrap и FWB.
- `mann_whitney_test()` — семантика `stats::wilcox.test()`, четыре вида
  гипотез, Hodges–Lehmann shift, аналитический и допустимый перестановочный
  вывод, ordinary bootstrap. Перестановочная equivalence явно запрещена,
  поскольку используемая статистика не является pivotal на границах нуля.
- `brunner_munzel_test()` — аналитический, logit и перестановочный вывод,
  relative effect, ordinary bootstrap и FWB.
- `kruskal_wallis_test()` — аналитический и перестановочный omnibus-тест,
  rank epsilon squared и стратифицированный ordinary bootstrap.
- `oneway_anova()` — классическая и Welch ANOVA, аналитический и
  перестановочный F-тест, eta squared и omega squared, ordinary bootstrap и
  FWB. Welch effect sizes явно маркируются как приближённые.
- Общие `bootstrap_control()` и `permutation_control()` фиксируют engines,
  число итераций, CI/p-value policy и seed до вычислений. Runtime fallback и
  неявный переход к точному перебору запрещены; внешний RNG восстанавливается.
- Все перечисленные функции терминальные: они поставляют тесты и effect sizes,
  но не внутригрупповые оценки, fitted objects или extractors.
- `run_comparison()` исполняет все пять терминальных функций напрямую из
  векторов или обычного data.frame/tibble, сам собирая внутренние data/context.
  Для metadata-aware и standalone путей используется один и тот же provider и
  одна схема результата. `print.bq_analysis_function()` показывает компактную
  specification вместо тела closure.
- `t_family()`, `mann_whitney_family()` и
  `brunner_munzel_family()` поставляют семейства `pairwise`, `reference` и
  `consecutive`; порядок factor задаёт направление и соседство, а
  character-группы всегда сортируются лексикографически. Коррекция
  множественности применяется только внутри объявленного семейства. Пока эти
  providers поддерживают только двусторонние аналитические варианты;
  directional, equivalence, permutation и bootstrap policy для семейства не
  спроектированы.
- `t_family(effect_size = ...)` по явному запросу поставляет `"cohens_d"` или
  `"hedges_g"`; default остаётся `"none"`. Student использует pooled SD,
  Welch — unpooled SD. Индивидуальный effect-size ДИ берётся из `effectsize`
  по noncentral-t методу и не соответствует multiplicity-adjusted p-value.
- `mann_whitney_family()` всегда поставляет Cliff's delta как
  `2 * U / (n_comparison * n_reference) - 1`. Основная оценка и ДИ остаются
  Hodges-Lehmann из `stats::wilcox.test()`; отдельный ДИ и стандартная ошибка
  для delta намеренно не добавлены и хранятся как typed NA.
- `brunner_munzel_family()` поставляет relative effect и Cliff's delta как
  `2 * relative_effect - 1`; ДИ delta и его стандартная ошибка являются тем же
  линейным преобразованием ДИ и SE relative effect.
- `tukey_test()` поставляет фиксированное all-pairs семейство с одновременными
  Tukey intervals. `dunnett_test()` поставляет many-to-one семейство,
  `games_howell_test()` — all-pairs семейство для неодинаковых дисперсий.
  `dunn_test()` поставляет rank-based `pairwise`, `reference` или
  `consecutive`. Последние три требуют Suggested `PMCMRplus (>= 1.9.12)` и не
  имеют runtime fallback.
- Все comparison-family providers являются отдельными от omnibus-тестов аналитическими
  сущностями и возвращают только стандартизированный плоский реестр
  `comparisons` и `sample_flow`. Они не вычисляют и не возвращают `tests` или
  `estimates`; omnibus-анализ объявляется и запускается отдельно. Направление
  оценки фиксировано как comparison minus reference.
- Общий `comparisons` schema содержит отдельные `std_error` для основной оценки
  и `effect_std_error` для размера эффекта, а также effect-size type, method,
  correction, CI bounds, level, scope и method. Числовая SE возвращается только
  когда она существует на шкале соответствующей оценки и согласована с методом
  интервала; иначе используется typed `NA_real_`. Сейчас это выполнено для
  разности средних в `t_family()`, relative effect и Cliff's delta в
  `brunner_munzel_family()`, а также разности средних в `tukey_test()`.
- Возможный будущий `test_family(test, comparisons, reference, p_adjust)`
  сможет компилировать поддерживаемую двухгрупповую specification в семейство.
  Пока конструктор не реализуется: сначала нужен общий контракт resampling и
  directional hypotheses. Настоящие post hoc процедуры `dunn_test()`,
  `tukey_test()`, `dunnett_test()` и `games_howell_test()` таким конструктором
  оборачиваться не должны.
- Permutation и bootstrap stages в `t_test()` и `mann_whitney_test()` имеют
  независимый RNG scope. Kruskal-Wallis и ANOVA проверяют конечность omnibus
  statistic/df/p-value и типизированно отклоняют вырожденные данные.

2037 тестов: FAIL 0, WARN 0, SKIP 0. Свежие `R CMD build` и
`R CMD check --no-manual` для текущего рабочего дерева — Status: OK. При
проверке было только сетевое сообщение о недоступном CRAN index, не NOTE/WARN.

## Следующий шаг

Следующую сессию начать с обсуждения и подтверждения оставшегося плана, не с
написания кода. Текущий рабочий список:

1. Решить и реализовать Cliff's delta для `dunn_test()`; по принятому правилу
   не придумывать отдельный ДИ, если процедура его не поставляет.
2. Отдельно спроектировать размеры эффекта, ДИ и SE для `tukey_test()` и
   `dunnett_test()`, сохраняя различие между индивидуальными и одновременными
   интервалами.
3. Отдельно спроектировать размер эффекта, ДИ и SE для
   `games_howell_test()` с учётом неодинаковых дисперсий.
4. После подтверждения численного контракта обновить README, NEWS, ROADMAP и
   этот handoff, затем снова выполнить полный `devtools::test()`, build и
   `R CMD check --no-manual`.
5. Только после завершения и явного разрешения автора создать коммит и push.

Resampling policy для семейств, directional/equivalence hypotheses и будущий
`test_family()` остаются отдельной последующей задачей. Более общий модельный
язык контрастов (`grand_mean`, произвольные коэффициенты,
interaction-контрасты) также не извлекается из терминальных тестов.

Для raw-vector описательных статистик встроенными остаются только
`continuous_descriptives()` и `continuous_descriptives_extended()`. Остальные
raw-vector статистики пользователь задаёт через `continuous_statistic()`.
Model-based estimands относятся к будущим функциям-поставщикам и не расширяют
этот набор. Raw Overall всегда вычисляется только по исходному вектору и
никогда не бывает model-based.

## Согласованный дизайн сравнительного анализа

- Центральная абстракция — функция-поставщик. Каждая аналитическая функция
  возвращает стандартизированный сформированный результат и fitted object
  либо реестр fitted objects с адресацией через `fit_id`.
- Функция-поставщик явно постулирует свои возможности: какие внутригрупповые
  оценки, сравнения и estimands она поставляет, а также поддержку ковариат,
  весов, кластеризации, matched sets, области подгонки и метода оценки
  неопределённости.
- Master model — роль конкретной подгоняемой модели, а не обязательная
  глобальная сущность и не автоматически выбранная наиболее сложная модель.
  Если модель объявлена master model, её fitted object можно переиспользовать
  для других результатов, которые функция-поставщик явно умеет извлекать.
  Произвольный downstream-код внутреннюю структуру fit не разбирает.
- Одна совместная модель `outcome ~ group * strata` может поставлять
  model-based summary и сравнения. Альтернативно отдельные модели
  `outcome ~ group` могут подгоняться внутри каждой страты и поставлять те же
  виды результатов в своих областях подгонки. Это разные явные стратегии, а
  не автоматически взаимозаменяемые реализации.
- Master model необязательна: прямые, design-based, взвешенные, matched и
  другие estimators могут поставлять результаты без неё. Estimand, estimator,
  fitted model и variance method остаются разными понятиями.
- `group` и каждая strata-переменная — полноценные самостоятельные оси
  дизайна. Несколько strata заранее не склеиваются в одну составную ось.
- Для каждой оси будущий запрос должен позволять contrast, condition, average
  или expand. Взаимодействия выражаются сочетанием контрастов нескольких осей;
  маргинализация требует явного правила весов.
- Пользовательские запросы компилируются в единое внутреннее представление —
  коэффициенты над ячейками полного пространства `group x strata`.
  Функция-поставщик получает уже скомпилированный контраст, независимо от того,
  как он был задан пользователем и каким методом будет оценён.
- Контракт должен в будущем вместить reference, все попарные, последовательные
  между соседними уровнями, последовательные относительно предыдущих уровней,
  отклонения от глобального среднего, произвольные коэффициенты и
  interaction-контрасты. Конкретные публичные конструкторы, названия и
  реализации этих семейств пока не приняты.
- Контрасты с глобальным средним должны хранить правило усреднения; оно не
  выводится молча из несбалансированных данных. Последовательные контрасты
  должны опираться на явный порядок уровней. Точные варианты и политики будут
  спроектированы позже.

## Известные пробелы

- `bind_rows()` и joins берут реестр **первого** аргумента; метаданные второго
  игнорируются молча. Слияния реестров с выявлением конфликтов нет.
- Дубликаты имён столбцов отвергает `tibble::as_tibble()` своей ошибкой, а не
  нашей `bq_error_*`. Единообразия типизации ошибок тут нет.
- Порог `max_levels = 20` в `infer_type()` произволен.
- Кодировка 1/2 (частая в выгрузках SPSS) не распознаётся как двухуровневая.
- `haven_labelled` не обрабатывается: `labelled` в зависимости не входит.
