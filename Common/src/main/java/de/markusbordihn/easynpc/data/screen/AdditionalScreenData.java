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
import de.markusbordihn.easynpc.data.attribute.BaseAttributes;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public class AdditionalScreenData {

  private static final String ACTION_EVENT_DATA_TAG = "ActionEventData";
  private static final String BASE_ATTRIBUTES_DATA_TAG = "BaseAttributesData";
  private static final String DIALOG_DATA_TAG = "DialogData";
  private static final String OBJECTIVE_DATA_TAG = "ObjectiveData";

  private final CompoundTag data;
  private final ActionEventSet actionEventSet;
  private final BaseAttributes baseAttributes;
  private final DialogDataSet dialogDataSet;
  private final ObjectiveDataSet objectiveDataSet;

  public AdditionalScreenData() {
    this(new CompoundTag());
  }

  public AdditionalScreenData(CompoundTag compoundTag) {
    // Processing know data.
    this.actionEventSet = getActionDataSet(compoundTag);
    this.baseAttributes = getBaseAttributes(compoundTag);
    this.dialogDataSet = getDialogDataSet(compoundTag);
    this.objectiveDataSet = getObjectiveDataSet(compoundTag);

    // Store data and remove already processed data.
    this.data = compoundTag;
    this.data.remove(ACTION_EVENT_DATA_TAG);
    this.data.remove(BASE_ATTRIBUTES_DATA_TAG);
    this.data.remove(DIALOG_DATA_TAG);
    this.data.remove(OBJECTIVE_DATA_TAG);
  }

  public static void addActionEventSet(CompoundTag compoundTag, EasyNPC<?> easyNPC) {
    if (compoundTag == null || easyNPC == null || easyNPC.getEasyNPCActionEventData() == null) {
      return;
    }
    compoundTag.put(
        ACTION_EVENT_DATA_TAG, easyNPC.getEasyNPCActionEventData().getActionEventSet().createTag());
  }

  public static ActionEventSet getActionDataSet(CompoundTag compoundTag) {
    if (!hasActionDataSet(compoundTag)) {
      return new ActionEventSet();
    }
    return new ActionEventSet(compoundTag.getCompound(ACTION_EVENT_DATA_TAG));
  }

  public static boolean hasActionDataSet(CompoundTag compoundTag) {
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

  public ActionEventSet getActionEventSet() {
    return this.actionEventSet;
  }

  public BaseAttributes getBaseAttributes() {
    return this.baseAttributes;
  }

  public DialogDataSet getDialogDataSet() {
    return this.dialogDataSet;
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
