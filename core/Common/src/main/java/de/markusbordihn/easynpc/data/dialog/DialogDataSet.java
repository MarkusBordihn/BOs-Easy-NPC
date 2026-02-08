/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.dialog;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.ConditionUtils;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.Scoreboard;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogDataSet {

  public static final String DATA_DIALOG_DATA_SET_TAG = "DialogDataSet";
  public static final String DATA_TYPE_TAG = "Type";
  public static final StreamCodec<RegistryFriendlyByteBuf, DialogDataSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public DialogDataSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new DialogDataSet(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, DialogDataSet dialogDataSet) {
          registryFriendlyByteBuf.writeNbt(
              EntityDataSerializersManager.validateAndGetNbt(
                  dialogDataSet.createTag(), "DialogDataSet"));
        }
      };
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private final HashMap<String, DialogDataEntry> dialogByLabelMap = new HashMap<>();
  private final HashMap<UUID, DialogDataEntry> dialogByIdMap = new HashMap<>();
  private DialogType dialogType = DialogType.STANDARD;

  public DialogDataSet() {}

  public DialogDataSet(DialogType dialogType) {
    this.dialogType = dialogType;
  }

  public DialogDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public void setDialog(UUID dialogId, DialogDataEntry dialogData) {
    if (dialogData == null) {
      log.error("Dialog data is null, please check your dialog data!");
      return;
    }
    if (this.hasDialog(dialogId)) {
      removeDialog(dialogId);
    }
    this.addDialog(dialogData);
  }

  public boolean addDialog(DialogDataEntry dialogData) {
    // Pre-check dialog data, before adding it to the dialog set.
    if (dialogData == null) {
      log.error("Dialog data is null, please check your dialog data!");
      return false;
    }
    if (dialogData.getId() == null) {
      log.error("Dialog id is null, please check your dialog data!");
      return false;
    }
    if (dialogData.getLabel() == null) {
      log.error("Dialog label is null, please check your dialog data!");
      return false;
    }
    if (dialogData.getText() == null || dialogData.getText().isEmpty()) {
      log.error("Dialog text is null or empty, please check your dialog data!");
      return false;
    }

    String dialogLabel = dialogData.getLabel();
    UUID dialogId = dialogData.getId();

    // Warn about duplicated dialog ids
    DialogDataEntry existingDialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (existingDialogData != null && !existingDialogData.equals(dialogData)) {
      log.warn(
          "Duplicated dialog with id {} found, will overwrite existing dialog {} with {}!",
          dialogId,
          dialogData,
          existingDialogData);
    }

    this.dialogByLabelMap.put(dialogLabel, dialogData);
    this.dialogByIdMap.put(dialogId, dialogData);
    return true;
  }

  public boolean removeDialog(UUID dialogId) {
    DialogDataEntry dialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (dialogData != null) {
      DialogDataEntry formerDialogData = this.dialogByIdMap.remove(dialogData.getId());
      if (formerDialogData != null) {
        this.dialogByLabelMap.remove(formerDialogData.getLabel());
      }
      return true;
    }
    return false;
  }

  public boolean removeDialogButton(UUID dialogId, UUID dialogButtonId) {
    DialogDataEntry dialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (dialogData != null) {
      return dialogData.removeDialogButton(dialogButtonId);
    }
    return false;
  }

  public List<DialogDataEntry> getDialogsByLabel() {
    return this.dialogByLabelMap.values().stream()
        .sorted(Comparator.comparing(DialogDataEntry::getLabel))
        .toList();
  }

  public Map<String, DialogDataEntry> getDialogByLabelMap() {
    return dialogByLabelMap;
  }

  public DialogDataEntry getDialog(String label) {
    return this.dialogByLabelMap.getOrDefault(label, null);
  }

  public DialogDataEntry getDialog(UUID id) {
    return this.dialogByIdMap.getOrDefault(id, null);
  }

  public UUID getDialogId(String dialogLabel) {
    DialogDataEntry dialogData = this.dialogByLabelMap.getOrDefault(dialogLabel, null);
    if (dialogData != null) {
      return dialogData.getId();
    }
    return null;
  }

  public boolean hasDialog() {
    return !this.dialogByLabelMap.isEmpty();
  }

  public boolean hasDialog(String label) {
    return this.dialogByLabelMap.containsKey(label);
  }

  public boolean hasDialog(UUID id) {
    return this.dialogByIdMap.containsKey(id);
  }

  public boolean hasDialogButton(UUID dialogId, UUID dialogButtonId) {
    return this.dialogByIdMap.containsKey(dialogId)
        && this.dialogByIdMap.get(dialogId).hasDialogButton(dialogButtonId);
  }

  public DialogButtonEntry getDialogButton(UUID dialogId, UUID dialogButtonId) {
    DialogDataEntry dialogData = this.dialogByIdMap.getOrDefault(dialogId, null);
    if (dialogData != null) {
      return dialogData.getDialogButton(dialogButtonId);
    }
    return null;
  }

  public DialogDataEntry getNextAvailableDialog(ServerPlayer serverPlayer) {
    return dialogByIdMap.values().stream()
        .filter(dialog -> dialog.getPriority() >= DialogPriority.FALLBACK)
        .filter(dialog -> checkConditions(dialog, serverPlayer))
        .sorted(
            Comparator.comparingInt(DialogDataEntry::getPriority)
                .reversed()
                .thenComparing(Comparator.comparing(DialogDataEntry::getLabel)))
        .findFirst()
        .orElse(null);
  }

  private boolean checkConditions(DialogDataEntry dialog, ServerPlayer serverPlayer) {
    if (!dialog.hasConditions()) {
      log.debug("Dialog {} has no conditions, allowing", dialog.getLabel());
      return true;
    }

    if (serverPlayer == null) {
      log.debug(
          "Cannot check conditions for dialog {} without player context, allowing dialog by default",
          dialog.getLabel());
      return true;
    }

    for (ConditionDataEntry condition : dialog.getConditions()) {
      if (!condition.isValid()) {
        log.debug("Skipping invalid condition {} for dialog {}", condition, dialog.getLabel());
        continue;
      }

      boolean conditionResult = evaluateCondition(condition, serverPlayer, dialog.getId());
      log.debug(
          "Condition check for dialog {}: {} {} {} = {} (result: {})",
          dialog.getLabel(),
          condition.conditionType(),
          condition.name(),
          condition.operationType().getSymbol() + " " + condition.value(),
          conditionResult ? "PASS" : "FAIL",
          conditionResult);

      if (!conditionResult) {
        log.debug(
            "Dialog {} rejected: condition not met ({} {} {} {})",
            dialog.getLabel(),
            condition.conditionType(),
            condition.name(),
            condition.operationType().getSymbol(),
            condition.value());
        return false;
      }
    }

    log.debug("Dialog {} accepted: all conditions passed", dialog.getLabel());
    return true;
  }

  public void recordDialogExecution(DialogDataEntry dialog, ServerPlayer serverPlayer) {
    if (dialog == null || serverPlayer == null || !dialog.hasConditions()) {
      return;
    }

    for (ConditionDataEntry condition : dialog.getConditions()) {
      if (condition.conditionType() == ConditionType.EXECUTION_LIMIT && condition.isValid()) {
        ConditionUtils.recordActionExecution(condition, serverPlayer, dialog.getId());
      }
    }
  }

  private boolean evaluateCondition(
      ConditionDataEntry condition, ServerPlayer serverPlayer, UUID dialogId) {
    return switch (condition.conditionType()) {
      case SCOREBOARD -> evaluateScoreboardCondition(condition, serverPlayer);
      case EXECUTION_LIMIT -> ConditionUtils.evaluateCondition(condition, serverPlayer, dialogId);
      case NONE -> {
        log.warn("Encountered NONE condition type, skipping");
        yield true;
      }
    };
  }

  private boolean evaluateScoreboardCondition(
      ConditionDataEntry condition, ServerPlayer serverPlayer) {
    if (!condition.hasName()) {
      log.warn("Scoreboard condition missing objective name!");
      return false;
    }

    int actualValue = -1;
    try {
      Scoreboard scoreboard = serverPlayer.getScoreboard();
      Objective objective = scoreboard.getObjective(condition.name());
      if (objective == null) {
        log.debug(
            "Scoreboard objective '{}' not found for player {}, using default value -1",
            condition.name(),
            serverPlayer.getName().getString());
      } else {
        actualValue = scoreboard.getOrCreatePlayerScore(serverPlayer, objective).get();
      }

      // Evaluate condition
      int expectedValue = condition.value();
      boolean result = condition.operationType().evaluate(actualValue, expectedValue);
      log.debug(
          "Scoreboard check: {} (actual: {}) {} {} (expected: {}) = {}",
          condition.name(),
          actualValue,
          condition.operationType().getSymbol(),
          expectedValue,
          expectedValue,
          result);

      return result;
    } catch (Exception e) {
      log.error("Error evaluating scoreboard condition for dialog: {}", condition, e);
      return false;
    }
  }

  public DialogType getType() {
    return this.dialogType;
  }

  public void load(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(DATA_DIALOG_DATA_SET_TAG)) {
      return;
    }

    // Load dialog type
    if (compoundTag.contains(DATA_TYPE_TAG)) {
      this.dialogType = DialogType.valueOf(compoundTag.getString(DATA_TYPE_TAG));
    }

    // Load dialog data
    this.dialogByLabelMap.clear();
    this.dialogByIdMap.clear();
    ListTag dialogListTag = compoundTag.getList(DATA_DIALOG_DATA_SET_TAG, 10);
    for (int i = 0; i < dialogListTag.size(); ++i) {
      CompoundTag dialogCompoundTag = dialogListTag.getCompound(i);
      DialogDataEntry dialogData = new DialogDataEntry(dialogCompoundTag);
      this.addDialog(dialogData);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag dialogListTag = new ListTag();
    for (DialogDataEntry dialogData : this.dialogByLabelMap.values()) {
      // Skip empty dialog data
      if (dialogData == null
          || dialogData.getId() == null
          || dialogData.getLabel() == null
          || dialogData.getText().isEmpty()) {
        continue;
      }
      dialogListTag.add(dialogData.createTag());
    }
    compoundTag.put(DATA_DIALOG_DATA_SET_TAG, dialogListTag);

    // Handle dialog type to avoid wrong dialog types after using the dialog editor.
    if ((this.dialogType == DialogType.BASIC && this.dialogByIdMap.size() > 1)
        || (this.dialogType == DialogType.YES_NO && this.dialogByIdMap.size() > 3)) {
      this.dialogType = DialogType.STANDARD;
    } else if (this.dialogByIdMap.isEmpty()) {
      this.dialogType = DialogType.NONE;
    } else if (this.dialogType != DialogType.BASIC
        && this.dialogType != DialogType.YES_NO
        && this.dialogType != DialogType.STANDARD) {
      this.dialogType = DialogType.CUSTOM;
    }
    compoundTag.putString(DATA_TYPE_TAG, this.dialogType.name());

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DialogDataSet [type=" + this.dialogType + ", " + this.dialogByLabelMap + "]";
  }
}
