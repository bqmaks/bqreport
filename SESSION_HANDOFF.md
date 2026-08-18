# SESSION_HANDOFF

Состояние на 2026-08-18 (вечер, после ревью проекта).

Контракт, инварианты, соглашения и принятый дизайн — в `AGENTS.md`. Здесь
только состояние и следующий шаг. Что делает каждый файл — в roxygen; что
менялось между версиями — в `NEWS.md`.

## Состояние git

- Ветка: `agent/stabilize-comparison-api`; `origin/main` отстаёт на пять
  коммитов сравнительного анализа.
- HEAD: `8bae5e2 Add multiple comparison families`.
- После HEAD — незакоммиченные правки по итогам ревью (см. ниже). Коммит и
  push делать только по новой явной просьбе автора; после коммита имеет
  смысл слить ветку в `main`.

## Карта пакета

- **Данные и метаданные.** `as_bq_data()`, реестры `variables` /
  `levels` / `summary_formats`, dplyr- и base-методы (`R/dplyr-methods.R`),
  `variables()`, `variable_levels()`, `summary_formats()`, `apply_dictionary()`,
  `infer_type()` / `infer_types()`, конструкторы типов `type_*()`, сеттеры
  `set_type()`, `set_role()`, `set_unit()`, `set_rounding()`,
  `set_summary_format()`; внутренний `resolve_variables()`.
- **Summary-конвейер.** `plan_summary()` → `add_statistic()` /
  `add_display_rule()` → `preflight()` → `run_analysis()` →
  `prepare_presentation()` → `format_presentation()` → `compose_table()`.
  Встроенные статистики `continuous_descriptives()` и
  `continuous_descriptives_extended()`, пользовательские —
  `continuous_statistic()`. `print()` у preflight/result/table и
  `as_tibble()` у table (широкая раскладка) — только для просмотра, реестры
  остаются источником истины. Сквозной пример — `demo/continuous-summary.R`.
- **Сравнение групп.** Пять терминальных тестов (`t_test()`,
  `mann_whitney_test()`, `brunner_munzel_test()`, `kruskal_wallis_test()`,
  `oneway_anova()`) и семь family/post-hoc providers (`t_family()`,
  `mann_whitney_family()`, `brunner_munzel_family()`, `dunn_test()`,
  `tukey_test()`, `dunnett_test()`, `games_howell_test()`). Исполняются
  через `run_comparison()`; общая проверка входа — `prepare_engine_input()`
  (`R/engine-input.R`), общие валидаторы — `R/checks.R`, `R/hypothesis.R`,
  `check_bootstrap_control()`, `check_permutation_control()`,
  `check_comparison_family()`.

## Сделано в этой сессии (ревью → правки)

- Переименованы конструкторы типов: `type_continuous()`, `type_count()`,
  `type_binary()`, `type_ordinal()`, `type_nominal()` (конфликт с
  `dplyr::count()`).
- Убрано дублирование в сравнительном слое: один `prepare_engine_input()`
  вместо пяти копий валидации data/context; контракт `context` — по именам,
  а не по порядку; общие `check_*()`; `resolve_hypothesis()` для margin /
  benefit; `check_bootstrap_control()` / `check_permutation_control()`
  проверяют структуру и наличие engine. Файлы providers сократились примерно
  вдвое (t-test.R 1080 → ~680 строк).
- `capabilities` сведены к читаемым полям: `outcome_types`,
  `group_min_levels`, `group_max_levels`, `supplied_results`,
  `suggested_dependencies`. `run_comparison()` теперь читает
  `outcome_types` (ordinal outcome для `t_test()` отклоняется) и
  `supplied_results` (нужен ли `estimate_id`).
- `run_comparison()`: tidyselect для `outcome`/`group` при `data`, numeric
  группы сортируются численно, единый контекст, результат класса
  `bq_result_comparison` с `analysis = "comparison"` и `specification`,
  compact print.
- Схема `comparisons`: `p_value` — сырое, `p_value_adjusted` — после
  коррекции; добавлены `ci_clamped` и `effect_ci_clamped`;
  `brunner_munzel_family()` больше не теряет флаг усечения TOSTER. Словарь
  `test` в `tests` без суффиксов `_test`/`_permutation` (`inference` хранит
  это отдельно): `student_t`, `welch_t`, `mann_whitney`, `hodges_lehmann`,
  `kruskal_wallis`, `oneway_anova`, `welch_anova`.
- `bq_data`: частичная замена строк (`x[i, j] <- value`) сохраняет реестр;
  замена столбца целиком по-прежнему сбрасывает value-metadata (общий
  `invalidate_value_metadata()`). `cbind()` и `merge()` отвергаются явно.
- Print-методы для preflight/result/table, `as_tibble.bq_table()`,
  snapshot-тесты (`tests/testthat/_snaps/`), `Config/testthat/parallel: true`.
- README, ROADMAP, NEWS обновлены; `README.md` больше не исключён из сборки,
  `ROADMAP.md` исключён.

2097 тестов: FAIL 0, WARN 0, SKIP 0. `R CMD build` и
`R CMD check --no-manual` для рабочего дерева — Status: OK, без NOTE.

## Следующий шаг

Автор выбрал начать со **стратификации сравнений** (там впервые появляется
master model), затем провести рефакторинг, затем перейти к другим
аналитическим типам (категориальные описательные и сравнения). Обсуждение
дизайна не завершено — начать следующую сессию с него, не с кода.

Предложение, ожидающее решений автора (сигнатуры — черновик):

- `plan_comparison(data, outcome, group, strata = NULL)` — конструктор
  дизайна, симметричный `plan_summary()`; те же `cells` / `cell_axes` в
  preflight (общий компилятор ячеек, а не своя копия).
- `add_comparison(plan, analysis, strata = "condition", weights = NULL)` —
  один запрос: provider + политика оси страт `"condition"` (внутри каждой
  страты) / `"average"` (усреднение по явному правилу) / `"ignore"` (пул);
  `weights` обязателен при `"average"`: `"equal"` или `"proportional"`, без
  молчаливого дефолта. `"expand"` и interaction-контрасты — следующий срез.
- `linear_model()` — первый master-model provider: `stats::lm` в cell-means
  параметризации `outcome ~ 0 + cell`; любой контраст — вектор `L` над
  ячейками (`L·β`, `L·V·Lᵀ`, `df_residual`); без `emmeans`/`multcomp`.
  Возвращает `fits` (`fit_id`, engine, формула, n, df, sigma), `estimates`
  (model-based средние ячеек — будущий источник model-based summary),
  `comparisons` (union-схема + `strata_scope`, `strata_value`,
  `weights_rule`, `fit_id`), `tests` (omnibus group и взаимодействие
  `group × strata` через `anova()`), `sample_flow` по ячейкам.
- Стратегия «терминальные тесты внутри каждой страты» (`t_test()` с
  `strata_var_id` в контексте) — тем же запросом, вторым шагом после
  `linear_model()`.
- Preflight: типы против `capabilities$outcome_types`, уровни, storage;
  пустая ячейка `group × strata` — blocking `empty_design_cell` для master
  model; `weights` без `"average"` — blocking.
- Множественность: коррекция внутри каждой страты; общая — только по явному
  `p_adjust_scope = "all"`, позже.

Открытые вопросы автору: (1) начинать с `linear_model()` или со стратегии
по стратам; (2) словарь `condition/average/ignore` и обязательный `weights`;
(3) пустая ячейка — блокировать или считать на неполной сетке; (4) первый
срез — одна strata-переменная; (5) model-based средние в `plan_summary` —
отдельный шаг позже; (6) что именно должен включать рефакторинг после среза
(общий `compile_design_cells`? общий сборщик engine frame?).

После стратификации: категориальные описательные (`n (%)`) и χ²/Fisher,
затем расширение post-hoc providers по порядку из ROADMAP. Resampling policy
для семейств, directional/equivalence и `test_family()` — отдельно.

## Известные пробелы

- `bind_rows()` и joins берут реестр **первого** аргумента; метаданные второго
  игнорируются молча. Слияния реестров с выявлением конфликтов нет.
- `tidyr::pivot_*()` возвращают plain tibble (как `summarise()`); отдельных
  методов нет.
- Дубликаты имён столбцов отвергает `tibble::as_tibble()` своей ошибкой, а не
  нашей `bq_error_*`.
- Порог `max_levels = 20` в `infer_type()` произволен.
- Кодировка 1/2 (частая в выгрузках SPSS) не распознаётся как двухуровневая.
- `haven_labelled` не обрабатывается: `labelled` в зависимости не входит.
- Схема `estimates` у двухгрупповых тестов различается по ширине (t_test —
  bootstrap-колонки, Mann-Whitney — нет); union-схема принята только для
  `comparisons`.
