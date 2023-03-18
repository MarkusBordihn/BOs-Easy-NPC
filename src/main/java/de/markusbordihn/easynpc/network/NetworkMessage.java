/**
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

package de.markusbordihn.easynpc.network;

import java.util.UUID;

import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.phys.Vec3;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.action.ActionType;
import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.entity.Profession;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.model.ModelPart;
import de.markusbordihn.easynpc.model.ModelPose;
import de.markusbordihn.easynpc.network.message.MessageActionChange;
import de.markusbordihn.easynpc.network.message.MessageActionDebug;
import de.markusbordihn.easynpc.network.message.MessageDialogTypeChange;
import de.markusbordihn.easynpc.network.message.MessageModelLockRotationChange;
import de.markusbordihn.easynpc.network.message.MessageModelPoseChange;
import de.markusbordihn.easynpc.network.message.MessageNameChange;
import de.markusbordihn.easynpc.network.message.MessageOpenConfiguration;
import de.markusbordihn.easynpc.network.message.MessagePoseChange;
import de.markusbordihn.easynpc.network.message.MessagePositionChange;
import de.markusbordihn.easynpc.network.message.MessageProfessionChange;
import de.markusbordihn.easynpc.network.message.MessageRemoveNPC;
import de.markusbordihn.easynpc.network.message.MessageRotationChange;
import de.markusbordihn.easynpc.network.message.MessageSaveBasicDialog;
import de.markusbordihn.easynpc.network.message.MessageSaveYesNoDialog;
import de.markusbordihn.easynpc.network.message.MessageScaleChange;
import de.markusbordihn.easynpc.network.message.MessageSkinChange;
import de.markusbordihn.easynpc.network.message.MessageTriggerAction;
import de.markusbordihn.easynpc.network.message.MessageVariantChange;
import de.markusbordihn.easynpc.skin.SkinType;

public class NetworkMessage {

  protected NetworkMessage() {}


  /** Send action change. */
  public static void actionChange(UUID uuid, ActionType actionType, String action) {
    if (uuid != null && actionType != null && actionType != ActionType.NONE) {
      NetworkHandler.sendToServer(new MessageActionChange(uuid, actionType.name(), action));
    }
  }

  /** Send action debug change. */
  public static void actionDebugChange(UUID uuid, boolean debug) {
    if (uuid != null) {
      NetworkHandler.sendToServer(new MessageActionDebug(uuid, debug));
    }
  }

  /** Send name change. */
  public static void nameChange(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandler.sendToServer(new MessageNameChange(uuid, name));
    }
  }

  /** Open configuration request. */
  public static void openConfiguration(UUID uuid, Enum<ConfigurationType> configurationType) {
    if (uuid != null && configurationType != null) {
      NetworkHandler.sendToServer(new MessageOpenConfiguration(uuid, configurationType.name()));
    }
  }

  /** Send model lock rotation change. */
  public static void modelLockRotationChange(UUID uuid, boolean lockRotation) {
    if (uuid != null) {
      NetworkHandler.sendToServer(new MessageModelLockRotationChange(uuid, lockRotation));
    }
  }

  /** Send model pose change. */
  public static void modelPoseChange(UUID uuid, ModelPose modelPose) {
    if (uuid != null && modelPose != null) {
      NetworkHandler.sendToServer(new MessageModelPoseChange(uuid, modelPose));
    }
  }

  /** Send pose change. */
  public static void poseChange(UUID uuid, Pose pose) {
    if (uuid != null && pose != null) {
      NetworkHandler.sendToServer(new MessagePoseChange(uuid, pose));
    }
  }

  /** Send position change. */
  public static void positionChange(UUID uuid, Vec3 pos) {
    if (uuid != null && pos != null) {
      NetworkHandler.sendToServer(new MessagePositionChange(uuid, pos));
    }
  }

  /** Send profession change. */
  public static void professionChange(UUID uuid, Profession profession) {
    if (uuid != null && profession != null) {
      NetworkHandler.sendToServer(new MessageProfessionChange(uuid, profession));
    }
  }

  /** Send remove NPC. */
  public static void removeNPC(UUID uuid) {
    if (uuid != null) {
      NetworkHandler.sendToServer(new MessageRemoveNPC(uuid));
    }
  }

  /** Send rotation change. */
  public static void rotationChange(UUID uuid, ModelPart modelPart, Rotations rotations) {
    if (uuid != null && modelPart != null && rotations != null) {
      NetworkHandler.sendToServer(new MessageRotationChange(uuid, modelPart, rotations));
    }
  }

  /** Change dialog type. */
  public static void changeDialogType(UUID uuid, DialogType dialogType) {
    if (uuid != null && dialogType != null) {
      NetworkHandler.sendToServer(new MessageDialogTypeChange(uuid, dialogType));
    }
  }

  /** Save basic dialog. */
  public static void saveBasicDialog(UUID uuid, String dialog) {
    if (uuid != null && dialog != null) {
      NetworkHandler.sendToServer(new MessageSaveBasicDialog(uuid, dialog));
    }
  }

  /** Save yes/no dialog. */
  public static void saveYesNoDialog(UUID uuid, String dialog, String yesDialog, String noDialog,
      String yesButtonText, String noButtonText) {
    if (uuid != null && dialog != null && yesDialog != null && noDialog != null) {
      NetworkHandler.sendToServer(new MessageSaveYesNoDialog(uuid, dialog, yesDialog, noDialog,
          yesButtonText, noButtonText));
    }
  }

  /** Send scale change. */
  public static void scaleChange(UUID uuid, String scaleAxis, float scale) {
    if (uuid != null && scaleAxis != null) {
      NetworkHandler.sendToServer(new MessageScaleChange(uuid, scaleAxis, scale));
    }
  }

  /** Send skin change. */
  public static void skinChange(UUID uuid, Enum<SkinType> skinType) {
    if (uuid != null && skinType != null) {
      NetworkHandler
          .sendToServer(new MessageSkinChange(uuid, "", "", Constants.BLANK_UUID, skinType.name()));
    }
  }

  public static void skinChange(UUID uuid, String skin, Enum<SkinType> skinType) {
    if (uuid != null && skin != null && skinType != null) {
      NetworkHandler.sendToServer(
          new MessageSkinChange(uuid, skin, "", Constants.BLANK_UUID, skinType.name()));
    }
  }

  public static void skinChange(UUID uuid, String skin, String skinURL, UUID skinUUID,
      Enum<SkinType> skinType) {
    if (uuid != null && skin != null && skinType != null) {
      NetworkHandler
          .sendToServer(new MessageSkinChange(uuid, skin, skinURL, skinUUID, skinType.name()));
    }
  }

  /** Send trigger action. */
  public static void triggerAction(UUID uuid, ActionType actionType) {
    if (uuid != null && actionType != null && actionType != ActionType.NONE) {
      NetworkHandler.sendToServer(new MessageTriggerAction(uuid, actionType.name()));
    }
  }

  /** Send variant change. */
  public static void variantChange(UUID uuid, Enum<?> variant) {
    if (uuid != null && variant != null) {
      NetworkHandler.sendToServer(new MessageVariantChange(uuid, variant.name()));
    }
  }

}
