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

package de.markusbordihn.easynpc.commands;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Command {

  public static final int FAILURE = 0;
  public static final int SINGLE_SUCCESS = 1;
  public static final String COLOR_ARG = "color";
  public static final String DIALOG_ARG = "dialog";
  public static final String ENABLE_ARG = "enable";
  public static final String ENTITY_ARG = "entity";
  public static final String ITEM_ARG = "item";
  public static final String MODEL_PART_ARG = "modelPart";
  public static final String NAME_ARG = "name";
  public static final String NPC_TARGET_ARG = "npc_target";
  public static final String NPC_TARGETS_ARG = "npc_targets";
  public static final String PARAMETER_ARG = "parameter";
  public static final String PLAYER_ARG = "player";
  public static final String POSITION_ARG = "position";
  public static final String SCALE_ARG = "scale";
  public static final String SLOT_ARG = "slot";
  public static final String SOUND_ARG = "sound";
  public static final String TARGET_ARG = "target";
  public static final String TYPE_ARG = "type";
  public static final String VALUE_ARG = "value";
  public static final String VARIANT_ARG = "variant";
  public static final String VISIBILITY_ARG = "visibility";
  public static final String X_ARG = "x";
  public static final String Y_ARG = "y";
  public static final String YAW_ARG = "yaw";
  public static final String Z_ARG = "z";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected Command() {}

  public static int sendFailureMessage(CommandSourceStack context, Component message) {
    context.sendFailure(message);
    return FAILURE;
  }

  public static int sendFailureMessage(CommandSourceStack context, String message) {
    context.sendFailure(TextComponent.getText(message));
    return FAILURE;
  }

  public static int sendSuccessMessage(CommandSourceStack context, Component message) {
    context.sendSuccess(() -> message, true);
    return SINGLE_SUCCESS;
  }

  public static int sendSuccessMessage(CommandSourceStack context, String message) {
    context.sendSuccess(() -> TextComponent.getText(message), true);
    return SINGLE_SUCCESS;
  }

  public static int sendSuccessMessage(
      CommandSourceStack context, String message, ChatFormatting formatting) {
    context.sendSuccess(() -> TextComponent.getText(message).withStyle(formatting), true);
    return SINGLE_SUCCESS;
  }

  public static int sendFailureMessageNoData(
      CommandSourceStack context, EasyNPC<?> easyNPC, String dataName) {
    return sendFailureMessage(
        context,
        "No " + dataName + " available for " + easyNPC + " with UUID " + easyNPC.getEntityUUID());
  }

  public static int sendFailureMessageNoNavigationData(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "navigation data");
  }

  public static int sendFailureMessageNoOwnerData(CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "owner data");
  }

  public static int sendFailureMessageNoSoundData(CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "sound data");
  }

  public static int sendFailureMessageNoTradingData(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "trading data");
  }

  public static int sendFailureMessageNoObjectiveData(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "objective data");
  }

  public static int sendFailureMessageNoObjectiveData(
      CommandSourceStack context, EasyNPC<?> easyNPC, String objectiveType) {
    return sendFailureMessageNoData(context, easyNPC, objectiveType + " objective data");
  }

  public static int sendFailureMessageNoMerchant(CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "merchant");
  }

  public static int sendFailureMessageNoSoundDataSet(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "sound data set");
  }

  public static int sendFailureMessageNoDialogData(CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendFailureMessageNoData(context, easyNPC, "dialog data");
  }
}
