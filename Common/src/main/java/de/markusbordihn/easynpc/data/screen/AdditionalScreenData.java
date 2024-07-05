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

package de.markusbordihn.easynpc.data.screen;

import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.attribute.BaseAttributes;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public class AdditionalScreenData {

  private static final String ACTION_EVENT_DATA_TAG = "ActionEventData";
  private static final String ACTION_EVENT_TYPE_TAG = "ActionEventType";
  private static final String BASE_ATTRIBUTES_DATA_TAG = "BaseAttributesData";
  private static final String CONFIGURATION_TYPE_TAG = "ConfigurationType";
  private static final String DIALOG_DATA_TAG = "DialogData";
  private static final String EDITOR_TYPE_TAG = "EditorType";
  private static final String OBJECTIVE_DATA_TAG = "ObjectiveData";

  private final ActionEventSet actionEventSet;
  private final ActionEventType actionEventType;
  private final BaseAttributes baseAttributes;
  private final CompoundTag data;
  private final ConfigurationType configurationType;
  private final DialogDataSet dialogDataSet;
  private final EditorType editorType;
  private final ObjectiveDataSet objectiveDataSet;

  public AdditionalScreenData(CompoundTag compoundTag) {
    // Processing know data.
    this.actionEventSet = getActionEventSet(compoundTag);
    this.actionEventType = getActionEventType(compoundTag);
    this.baseAttributes = getBaseAttributes(compoundTag);
    this.configurationType = getConfigurationType(compoundTag);
    this.dialogDataSet = getDialogDataSet(compoundTag);
    this.editorType = getEditorType(compoundTag);
    this.objectiveDataSet = getObjectiveDataSet(compoundTag);

    // Store data and remove already processed data.
    this.data = compoundTag;
    this.data.remove(ACTION_EVENT_DATA_TAG);
    this.data.remove(ACTION_EVENT_TYPE_TAG);
    this.data.remove(BASE_ATTRIBUTES_DATA_TAG);
    this.data.remove(CONFIGURATION_TYPE_TAG);
    this.data.remove(DIALOG_DATA_TAG);
    this.data.remove(EDITOR_TYPE_TAG);
    this.data.remove(OBJECTIVE_DATA_TAG);
  }

  public static void addActionEventType(CompoundTag compoundTag, ActionEventType actionEventType) {
    if (compoundTag == null || actionEventType == null) {
      return;
    }
    compoundTag.putString(ACTION_EVENT_TYPE_TAG, actionEventType.name());
  }

  public static ActionEventType getActionEventType(CompoundTag compoundTag) {
    if (!hasActionEventType(compoundTag)) {
      return ActionEventType.NONE;
    }
    return ActionEventType.get(compoundTag.getString(ACTION_EVENT_TYPE_TAG));
  }

  public static boolean hasActionEventType(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(ACTION_EVENT_TYPE_TAG);
  }

  public static void addActionEventSet(CompoundTag compoundTag, EasyNPC<?> easyNPC) {
    if (compoundTag == null || easyNPC == null || easyNPC.getEasyNPCActionEventData() == null) {
      return;
    }
    compoundTag.put(
        ACTION_EVENT_DATA_TAG, easyNPC.getEasyNPCActionEventData().getActionEventSet().createTag());
  }

  public static ActionEventSet getActionEventSet(CompoundTag compoundTag) {
    if (!hasActionEventSet(compoundTag)) {
      return new ActionEventSet();
    }
    return new ActionEventSet(compoundTag.getCompound(ACTION_EVENT_DATA_TAG));
  }

  public static boolean hasActionEventSet(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(ACTION_EVENT_DATA_TAG);
  }

  public static void addBaseAttributes(CompoundTag compoundTag, EasyNPC<?> easyNPC) {
    if (compoundTag == null || easyNPC == null || easyNPC.getLivingEntity() == null) {
      return;
    }
    compoundTag.put(
        BASE_ATTRIBUTES_DATA_TAG, new BaseAttributes(easyNPC.getLivingEntity()).createTag());
  }

  public static BaseAttributes getBaseAttributes(CompoundTag compoundTag) {
    if (!hasBaseAttributes(compoundTag)) {
      return new BaseAttributes();
    }
    return new BaseAttributes(compoundTag.getCompound(BASE_ATTRIBUTES_DATA_TAG));
  }

  public static boolean hasBaseAttributes(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(BASE_ATTRIBUTES_DATA_TAG);
  }

  public static void addConfigurationType(
      CompoundTag compoundTag, ConfigurationType configurationType) {
    if (compoundTag == null || configurationType == null) {
      return;
    }
    compoundTag.putString(CONFIGURATION_TYPE_TAG, configurationType.name());
  }

  public static ConfigurationType getConfigurationType(CompoundTag compoundTag) {
    if (!hasConfigurationType(compoundTag)) {
      return ConfigurationType.NONE;
    }
    return ConfigurationType.get(compoundTag.getString(CONFIGURATION_TYPE_TAG));
  }

  public static boolean hasConfigurationType(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(CONFIGURATION_TYPE_TAG);
  }

  public static void addDialogDataSet(CompoundTag compoundTag, EasyNPC<?> easyNPC) {
    if (compoundTag == null || easyNPC == null || easyNPC.getEasyNPCDialogData() == null) {
      return;
    }
    compoundTag.put(DIALOG_DATA_TAG, easyNPC.getEasyNPCDialogData().getDialogDataSet().createTag());
  }

  public static DialogDataSet getDialogDataSet(CompoundTag compoundTag) {
    if (!hasDialogDataSet(compoundTag)) {
      return new DialogDataSet();
    }
    return new DialogDataSet(compoundTag.getCompound(DIALOG_DATA_TAG));
  }

  public static boolean hasDialogDataSet(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(DIALOG_DATA_TAG);
  }

  public static void addEditorType(CompoundTag compoundTag, EditorType editorType) {
    if (compoundTag == null || editorType == null) {
      return;
    }
    compoundTag.putString(EDITOR_TYPE_TAG, editorType.name());
  }

  public static EditorType getEditorType(CompoundTag compoundTag) {
    if (!hasEditorType(compoundTag)) {
      return EditorType.NONE;
    }
    return EditorType.get(compoundTag.getString(EDITOR_TYPE_TAG));
  }

  public static boolean hasEditorType(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(EDITOR_TYPE_TAG);
  }

  public static void addObjectiveDataSet(CompoundTag compoundTag, EasyNPC<?> easyNPC) {
    if (compoundTag == null || easyNPC == null || easyNPC.getEasyNPCObjectiveData() == null) {
      return;
    }
    compoundTag.put(
        OBJECTIVE_DATA_TAG, easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet().createTag());
  }

  public static ObjectiveDataSet getObjectiveDataSet(CompoundTag compoundTag) {
    if (!hasObjectiveDataSet(compoundTag)) {
      return new ObjectiveDataSet();
    }
    return new ObjectiveDataSet(compoundTag.getCompound(OBJECTIVE_DATA_TAG));
  }

  public static boolean hasObjectiveDataSet(CompoundTag compoundTag) {
    return compoundTag != null && compoundTag.contains(OBJECTIVE_DATA_TAG);
  }

  public ActionEventType getActionEventType() {
    return this.actionEventType;
  }

  public ActionEventSet getActionEventSet() {
    return this.actionEventSet;
  }

  public BaseAttributes getBaseAttributes() {
    return this.baseAttributes;
  }

  public ConfigurationType getConfigurationType() {
    return this.configurationType;
  }

  public DialogDataSet getDialogDataSet() {
    return this.dialogDataSet;
  }

  public EditorType getEditorType() {
    return this.editorType;
  }

  public ObjectiveDataSet getObjectiveDataSet() {
    return this.objectiveDataSet;
  }

  public CompoundTag getData() {
    return this.data;
  }

  public CompoundTag get(String dataTag) {
    if (this.data.contains(dataTag)) {
      return this.data.getCompound(dataTag);
    }
    return new CompoundTag();
  }

  public ListTag getList(String dataTag) {
    if (this.data.contains(dataTag)) {
      return this.data.getList(dataTag, 10);
    }
    return new ListTag();
  }
}
