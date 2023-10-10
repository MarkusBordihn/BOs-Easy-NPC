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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.core.Rotations;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.phys.Vec3;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.entity.Profession;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.message.MessageActionChange;

import de.markusbordihn.easynpc.network.message.MessageDialogTypeChange;
import de.markusbordihn.easynpc.network.message.MessageEntityAttributeChange;
import de.markusbordihn.easynpc.network.message.MessageModelLockRotationChange;
import de.markusbordihn.easynpc.network.message.MessageModelPoseChange;
import de.markusbordihn.easynpc.network.message.MessageModelPositionChange;
import de.markusbordihn.easynpc.network.message.MessageModelVisibilityChange;
import de.markusbordihn.easynpc.network.message.MessageNameChange;
import de.markusbordihn.easynpc.network.message.MessageOpenConfiguration;
import de.markusbordihn.easynpc.network.message.MessagePoseChange;
import de.markusbordihn.easynpc.network.message.MessagePositionChange;
import de.markusbordihn.easynpc.network.message.MessagePresetExport;
import de.markusbordihn.easynpc.network.message.MessagePresetExportClient;
import de.markusbordihn.easynpc.network.message.MessagePresetExportWorld;
import de.markusbordihn.easynpc.network.message.MessagePresetImport;
import de.markusbordihn.easynpc.network.message.MessagePresetImportWorld;
import de.markusbordihn.easynpc.network.message.MessageProfessionChange;
import de.markusbordihn.easynpc.network.message.MessageRemoveNPC;
import de.markusbordihn.easynpc.network.message.MessageRotationChange;
import de.markusbordihn.easynpc.network.message.MessageSaveBasicDialog;
import de.markusbordihn.easynpc.network.message.MessageSaveYesNoDialog;
import de.markusbordihn.easynpc.network.message.MessageScaleChange;
import de.markusbordihn.easynpc.network.message.MessageSkinChange;
import de.markusbordihn.easynpc.network.message.MessageTradingTypeChange;
import de.markusbordihn.easynpc.network.message.MessageTriggerAction;
import de.markusbordihn.easynpc.network.message.MessageVariantChange;

public class NetworkMessageHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected NetworkMessageHandler() {}

  /** Send action change. */
  public static void actionChange(UUID uuid, ActionData actionData) {
    if (uuid != null && actionData != null && actionData.isValid()) {
      NetworkHandler.sendToServer(new MessageActionChange(uuid, actionData));
    }
  }

  /** Send name change. */
  public static void nameChange(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandler.sendToServer(new MessageNameChange(uuid, name));
    }
  }

  /** Open configuration request. */
  public static void openConfiguration(UUID uuid, ConfigurationType configurationType) {
    if (uuid != null && configurationType != null) {
      NetworkHandler.sendToServer(new MessageOpenConfiguration(uuid, configurationType));
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

  /** Send position change. */
  public static void modelPositionChange(UUID uuid, ModelPart modelPart, CustomPosition position) {
    if (uuid != null && modelPart != null && position != null) {
      NetworkHandler.sendToServer(new MessageModelPositionChange(uuid, modelPart, position));
    }
  }

  /** Send visibility change. */
  public static void modelVisibilityChange(UUID uuid, ModelPart modelPart, boolean visible) {
    if (uuid != null && modelPart != null) {
      NetworkHandler.sendToServer(new MessageModelVisibilityChange(uuid, modelPart, visible));
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
  public static void skinChange(UUID uuid, SkinType skinType) {
    if (uuid != null && skinType != null) {
      NetworkHandler
          .sendToServer(new MessageSkinChange(uuid, "", "", Constants.BLANK_UUID, skinType));
    }
  }

  public static void skinChange(UUID uuid, String skin, SkinType skinType) {
    if (uuid != null && skin != null && skinType != null) {
      NetworkHandler
          .sendToServer(new MessageSkinChange(uuid, skin, "", Constants.BLANK_UUID, skinType));
    }
  }

  public static void skinChange(UUID uuid, String skin, String skinURL, UUID skinUUID,
      SkinType skinType) {
    if (uuid != null && skin != null && skinType != null) {
      NetworkHandler.sendToServer(new MessageSkinChange(uuid, skin, skinURL, skinUUID, skinType));
    }
  }

  /** Send trigger action. */
  public static void triggerAction(UUID uuid, ActionType actionType) {
    if (uuid != null && actionType != null && actionType != ActionType.NONE) {
      NetworkHandler.sendToServer(new MessageTriggerAction(uuid, actionType));
    }
  }

  /** Send variant change. */
  public static void variantChange(UUID uuid, Enum<?> variant) {
    if (uuid != null && variant != null) {
      NetworkHandler.sendToServer(new MessageVariantChange(uuid, variant.name()));
    }
  }

  /** Export preset to player */
  public static void exportPreset(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandler.sendToServer(new MessagePresetExport(uuid, name));
    }
  }

  /** Export preset to player */
  public static void exportPresetWorld(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandler.sendToServer(new MessagePresetExportWorld(uuid, name));
    }
  }

  /** Export preset to player */
  public static void exportPresetClient(UUID uuid, String name, ServerPlayer serverPlayer) {
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (uuid != null && easyNPCEntity != null) {
      log.info("Exporting preset for {} to {}", easyNPCEntity.getName().getString(),
          serverPlayer.getName().getString());
      NetworkHandler
          .sendToPlayer(new MessagePresetExportClient(uuid, easyNPCEntity.getName().getString(),
              easyNPCEntity.getSkinModel(), name, easyNPCEntity.exportPreset()), serverPlayer);
    }
  }

  public static void exportPresetClient(UUID uuid, ServerPlayer serverPlayer) {
    exportPresetClient(uuid, uuid.toString(), serverPlayer);
  }

  /** Import preset from player */
  public static void importWorldPreset(UUID uuid, ResourceLocation resourceLocation) {
    if (uuid != null && resourceLocation != null) {
      NetworkHandler.sendToServer(new MessagePresetImportWorld(uuid, resourceLocation));
    }
  }

  public static void importPreset(UUID uuid, CompoundTag compoundTag) {
    if (uuid != null && compoundTag != null) {
      NetworkHandler.sendToServer(new MessagePresetImport(uuid, compoundTag));
    }
  }

  /** Entity Attribute Change */
  public static void entityAttributeChange(UUID uuid, EntityAttribute entityAttribute,
      Boolean booleanValue) {
    if (uuid != null && entityAttribute != null && booleanValue != null) {
      NetworkHandler
          .sendToServer(new MessageEntityAttributeChange(uuid, entityAttribute, booleanValue));
    }
  }

  public static void entityAttributeChange(UUID uuid, EntityAttribute entityAttribute,
      Float floatValue) {
    if (uuid != null && entityAttribute != null && floatValue != null) {
      NetworkHandler
          .sendToServer(new MessageEntityAttributeChange(uuid, entityAttribute, floatValue));
    }
  }

  public static void entityAttributeChange(UUID uuid, EntityAttribute entityAttribute,
      Integer integerValue) {
    if (uuid != null && entityAttribute != null && integerValue != null) {
      NetworkHandler
          .sendToServer(new MessageEntityAttributeChange(uuid, entityAttribute, integerValue));
    }
  }

  public static void entityAttributeChange(UUID uuid, EntityAttribute entityAttribute,
      String stringValue) {
    if (uuid != null && entityAttribute != null && stringValue != null) {
      NetworkHandler
          .sendToServer(new MessageEntityAttributeChange(uuid, entityAttribute, stringValue));
    }
  }

  /** Change trading type. */
  public static void changeTradingType(UUID uuid, TradingType tradingType) {
    if (uuid != null && tradingType != null) {
      NetworkHandler.sendToServer(new MessageTradingTypeChange(uuid, tradingType));
    }
  }

}
