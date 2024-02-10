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

package de.markusbordihn.easynpc.entity.easynpc.data.legacy;

import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public class LegacyDialogSetData {

  protected LegacyDialogSetData() {}

  public static ActionData readAdditionalLegacyActionData(
      CompoundTag compoundTag, DialogDataSet dialogDataSet, String actionType) {
    if (actionType == null
        || !compoundTag.contains(ActionEventData.DATA_ACTION_PERMISSION_LEVEL_TAG)
        || !compoundTag.contains(ActionEventData.DATA_ACTION_DATA_TAG)) {
      return null;
    }
    CompoundTag actionDataTag = compoundTag.getCompound(ActionEventData.DATA_ACTION_DATA_TAG);
    DialogDataEntry questionDialogData = dialogDataSet.getDialog("question");
    if (questionDialogData != null && actionDataTag.contains(ActionEventData.DATA_ACTIONS_TAG)) {
      ListTag listTag = actionDataTag.getList(ActionEventData.DATA_ACTIONS_TAG, 10);
      for (int i = 0; i < listTag.size(); ++i) {
        CompoundTag actionTag = listTag.getCompound(i);
        String actionEventType = actionTag.getString(ActionEventData.DATA_ACTION_TYPE_TAG);
        if (actionType.equals(actionEventType)) {
          String actionCommand = actionTag.getString(ActionEventData.DATA_ACTION_TAG);
          if (actionCommand.equals("/easy_npc trading open @npc-uuid")) {
            return new ActionData(
                ActionType.OPEN_TRADING_SCREEN,
                actionTag.getString(""),
                actionTag.getInt(ActionEventData.DATA_ACTION_PERMISSION_LEVEL_TAG),
                actionTag.getBoolean(ActionEventData.DATA_ACTION_EXECUTE_AS_USER_TAG),
                actionTag.getBoolean(ActionEventData.DATA_ACTION_ENABLE_DEBUG_TAG));
          } else {
            return new ActionData(
                ActionType.COMMAND,
                actionTag.getString(actionCommand),
                actionTag.getInt(ActionEventData.DATA_ACTION_PERMISSION_LEVEL_TAG),
                actionTag.getBoolean(ActionEventData.DATA_ACTION_EXECUTE_AS_USER_TAG),
                actionTag.getBoolean(ActionEventData.DATA_ACTION_ENABLE_DEBUG_TAG));
          }
        }
      }
    }
    return null;
  }
}
