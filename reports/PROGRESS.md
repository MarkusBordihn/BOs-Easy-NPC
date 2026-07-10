# EasyNPC Advanced Rig Progress

## Этап 0 — baseline и аудит EME pose rendering

### Выполнено

- Подтверждены ветки `26.1.2` и `1.20.1` для EasyNPC и Easy Model Entities.
- Зафиксированы исходные commit SHA всех трёх репозиториев.
- Подтверждена фактическая структура `core`, `config-ui`, `bundle` и loader-модулей.
- Прослежен submit-node render pipeline Minecraft 26.1.2.
- Подтверждена регрессия: submit-path EME всегда передавал `EasyModelPartAnimator.NONE`.
- Подготовлен минимальный межрепозиторный patch для передачи render options через render state.
- В EasyNPC восстановлено сопоставление EME bone name с существующим `ModelPartType`.
- Animator получает immutable snapshot pose-данных на этапе `extractRenderState` и не читает entity в draw-call.
- При повторном использовании render state очищаются model, animation и movement поля, чтобы исключить stale state.
- Созданы commits: EME `132b2a8`, EasyNPC `9b36baa6`.

### Изменённые файлы

Easy Model Entities:

- `Common/src/main/java/de/markusbordihn/easymodelentities/client/render/EasyModelEntityRenderState.java`
- `Common/src/main/java/de/markusbordihn/easymodelentities/client/render/EasyModelEntityRenderBackend.java`
- `Common/src/test/java/de/markusbordihn/easymodelentities/client/render/EasyModelEntityRenderBackendTest.java`

EasyNPC:

- `core/Common/src/main/java/de/markusbordihn/easynpc/client/renderer/entity/easymodelentities/EasyModelNPCPartAnimator.java`
- `core/Common/src/test/java/de/markusbordihn/easynpc/client/renderer/entity/easymodelentities/EasyModelNPCPartAnimatorTest.java`
- `core/Fabric/src/main/java/de/markusbordihn/easynpc/client/renderer/entity/easymodelentities/EasyModelNPCRenderer.java`
- `core/Forge/src/main/java/de/markusbordihn/easynpc/client/renderer/entity/easymodelentities/EasyModelNPCRenderer.java`
- `core/NeoForge/src/main/java/de/markusbordihn/easynpc/client/renderer/entity/easymodelentities/EasyModelNPCRenderer.java`

### Архитектурные решения

- Render options являются частью EME render state и потребляются существующим backend вместо жёстко заданного `NONE + ADD`.
- EasyNPC сохраняет старую семантику additive pose поверх автоматической EME animation.
- В render state передаётся snapshot готовых `EasyModelPartTransform`, а не ссылка на NPC или синхронизируемые maps.
- Не вводятся dynamic bones, новый entity type или UI до проверки первого milestone.

### Проверки

- `git diff --check`: успешно для EasyNPC и EME.
- Добавлены unit tests для default/custom render options.
- Добавлены unit tests для canonical bone mapping, unknown bone fallback и snapshot isolation.
- Baseline/patch Gradle build не выполнен по причине окружения: доступна только Java 17 вместо Java 25; Gradle wrapper 9.6.1 не может скачать distribution из-за сетевого ограничения.

### Известные проблемы

- Patch требует совместных совместимых версий EME и EasyNPC; EasyNPC нельзя собирать против старого бинарника EME 1.5.0 без нового поля render state.
- Не выполнены client, dedicated server и in-game проверки.
- Не проверены root transforms и GUI preview; первый patch восстанавливает только существующие именованные `ModelPartType` bones.

### Следующий шаг

1. Собрать EME на Java 25 и опубликовать в local Maven либо передать EasyNPC через `easy_model_entities_local_jar`.
2. Собрать `core`, затем `config-ui`, затем `bundle`.
3. Запустить Fabric client и проверить rotation/position/scale/visibility на стандартной EME humanoid-модели.
4. Запустить dedicated Fabric server и multiplayer sync test.
5. После зелёного milestone перейти к dynamic bone data.
