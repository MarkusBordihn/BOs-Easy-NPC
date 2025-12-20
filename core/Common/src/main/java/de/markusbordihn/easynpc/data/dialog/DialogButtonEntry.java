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

import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;

public record DialogButtonEntry(
    UUID id,
    String name,
    String label,
    DialogButtonType type,
    ActionDataSet actionDataSet,
    Set<ConditionDataEntry> conditions,
    boolean isTranslationKey) {

  public static final String DATA_ACTIONS_TAG = "Actions";
  public static final String DATA_CONDITIONS_TAG = "Conditions";
  public static final String DATA_BUTTON_NAME_TAG = "Name";
  public static final String DATA_LABEL_TAG = "Label";
  public static final String DATA_TYPE_TAG = "Type";
  public static final int MAX_BUTTON_LABEL_LENGTH = 32;

  public DialogButtonEntry(CompoundTag compoundTag) {
    this(
        compoundTag.getString(DATA_BUTTON_NAME_TAG),
        compoundTag.getString(DATA_LABEL_TAG),
        DialogButtonType.get(compoundTag.getString(DATA_TYPE_TAG)),
        new ActionDataSet(compoundTag, DATA_ACTIONS_TAG),
        loadConditions(compoundTag));
  }

  public DialogButtonEntry(String name, String label, ActionDataSet actionDataSet) {
    this(name, label, DialogButtonType.DEFAULT, actionDataSet, new LinkedHashSet<>());
  }

  public DialogButtonEntry(String name, DialogButtonType type) {
    this(name, null, type, new ActionDataSet(), new LinkedHashSet<>());
  }

  public DialogButtonEntry(
      String name, String label, DialogButtonType type, ActionDataSet actionDataSet) {
    this(name, label, type, actionDataSet, new LinkedHashSet<>());
  }

  public DialogButtonEntry(
      String name,
      String label,
      DialogButtonType type,
      ActionDataSet actionDataSet,
      Set<ConditionDataEntry> conditions) {
    this(
        UUIDUtils.textToUUID(
            label != null && !label.isEmpty() ? label : DialogUtils.generateButtonLabel(name)),
        name,
        label != null && !label.isEmpty() ? label : DialogUtils.generateButtonLabel(name),
        type != null ? type : DialogButtonType.DEFAULT,
        actionDataSet != null ? actionDataSet : new ActionDataSet(),
        conditions != null ? conditions : new LinkedHashSet<>(),
        TextUtils.isTranslationKey(name));
  }

  private static Set<ConditionDataEntry> loadConditions(CompoundTag compoundTag) {
    Set<ConditionDataEntry> conditions = new LinkedHashSet<>();
    if (compoundTag.contains(DATA_CONDITIONS_TAG)) {
      ListTag conditionsList = compoundTag.getList(DATA_CONDITIONS_TAG, 10);
      for (int i = 0; i < conditionsList.size(); i++) {
        ConditionDataEntry condition = new ConditionDataEntry(conditionsList.getCompound(i));
        if (condition.isValid()) {
          conditions.add(condition);
        }
      }
    }
    return conditions;
  }

  public Component getButtonName(int maxLength) {
    Component buttonName = TextComponent.getTextComponentRaw(this.name, isTranslationKey);
    if (buttonName.getString().length() > maxLength) {
      buttonName = TextComponent.getText(buttonName.getString().substring(0, maxLength - 1) + '…');
    }
    return buttonName;
  }

  public boolean hasActionData() {
    return actionDataSet != null && actionDataSet.hasActionData();
  }

  public boolean hasConditions() {
    return conditions != null && !conditions.isEmpty();
  }

  public DialogButtonEntry withName(String name) {
    return new DialogButtonEntry(
        this.id,
        name,
        this.label,
        this.type,
        this.actionDataSet,
        this.conditions,
        TextUtils.isTranslationKey(name));
  }

  public DialogButtonEntry withLabel(String label) {
    return new DialogButtonEntry(
        UUIDUtils.textToUUID(label != null && !label.isEmpty() ? label : name),
        this.name,
        label,
        this.type,
        this.actionDataSet,
        this.conditions,
        this.isTranslationKey);
  }

  public DialogButtonEntry withActionDataSet(ActionDataSet actionDataSet) {
    return new DialogButtonEntry(
        this.id,
        this.name,
        this.label,
        this.type,
        actionDataSet != null ? actionDataSet : new ActionDataSet(),
        this.conditions,
        this.isTranslationKey);
  }

  public DialogButtonEntry withConditions(Set<ConditionDataEntry> conditions) {
    return new DialogButtonEntry(
        this.id,
        this.name,
        this.label,
        this.type,
        this.actionDataSet,
        conditions != null ? conditions : new LinkedHashSet<>(),
        this.isTranslationKey);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putString(DATA_BUTTON_NAME_TAG, this.name.trim());

    // Only save type if it is different from default.
    if (this.type != DialogButtonType.DEFAULT) {
      compoundTag.putString(DATA_TYPE_TAG, this.type.name());
    }

    // Only save label if it is different from auto-generated label.
    if (this.label != null && !Objects.equals(DialogUtils.generateButtonLabel(name), this.label)) {
      compoundTag.putString(DATA_LABEL_TAG, this.label);
    }

    // Save action data
    this.actionDataSet.save(compoundTag, DATA_ACTIONS_TAG);

    // Save conditions, if any.
    if (this.conditions != null && !this.conditions.isEmpty()) {
      ListTag conditionsList = new ListTag();
      for (ConditionDataEntry condition : this.conditions) {
        if (condition.isValid()) {
          conditionsList.add(condition.createTag());
        }
      }
      if (!conditionsList.isEmpty()) {
        compoundTag.put(DATA_CONDITIONS_TAG, conditionsList);
      }
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.write(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DialogButtonData [id="
        + this.id
        + ", name="
        + this.name
        + ", label="
        + this.label
        + ", type="
        + this.type
        + ", isTranslationKey="
        + this.isTranslationKey
        + ", actionDataSet="
        + this.actionDataSet
        + ", conditions="
        + this.conditions
        + "]";
  }
}
