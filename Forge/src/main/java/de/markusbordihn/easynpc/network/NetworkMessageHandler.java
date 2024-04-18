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

package de.markusbordihn.easynpc.network;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.spawner.SpawnerSettingType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.message.ChangeSpawnerSettingMessage;
import de.markusbordihn.easynpc.network.message.MessageActionEventChange;
import de.markusbordihn.easynpc.network.message.MessageAdvancedTrading;
import de.markusbordihn.easynpc.network.message.MessageBasicTrading;
import de.markusbordihn.easynpc.network.message.MessageDialogButtonAction;
import de.markusbordihn.easynpc.network.message.MessageEntityAttributeChange;
import de.markusbordihn.easynpc.network.message.MessageEntityBaseAttributeChange;
import de.markusbordihn.easynpc.network.message.MessageModelEquipmentVisibilityChange;
import de.markusbordihn.easynpc.network.message.MessageModelLockRotationChange;
import de.markusbordihn.easynpc.network.message.MessageModelPoseChange;
import de.markusbordihn.easynpc.network.message.MessageModelPositionChange;
import de.markusbordihn.easynpc.network.message.MessageModelRotationChange;
import de.markusbordihn.easynpc.network.message.MessageModelVisibilityChange;
import de.markusbordihn.easynpc.network.message.MessageNameChange;
import de.markusbordihn.easynpc.network.message.MessageObjectiveAdd;
import de.markusbordihn.easynpc.network.message.MessageObjectiveRemove;
import de.markusbordihn.easynpc.network.message.MessageOpenConfiguration;
import de.markusbordihn.easynpc.network.message.MessageOpenDialog;
import de.markusbordihn.easynpc.network.message.MessageOpenDialogButtonEditor;
import de.markusbordihn.easynpc.network.message.MessageOpenDialogEditor;
import de.markusbordihn.easynpc.network.message.MessageOpenDialogTextEditor;
import de.markusbordihn.easynpc.network.message.MessagePoseChange;
import de.markusbordihn.easynpc.network.message.MessagePositionChange;
import de.markusbordihn.easynpc.network.message.MessagePresetExport;
import de.markusbordihn.easynpc.network.message.MessagePresetExportClient;
import de.markusbordihn.easynpc.network.message.MessagePresetExportWorld;
import de.markusbordihn.easynpc.network.message.MessagePresetImport;
import de.markusbordihn.easynpc.network.message.MessagePresetImportWorld;
import de.markusbordihn.easynpc.network.message.MessageProfessionChange;
import de.markusbordihn.easynpc.network.message.MessageRemoveDialog;
import de.markusbordihn.easynpc.network.message.MessageRemoveDialogButton;
import de.markusbordihn.easynpc.network.message.MessageRemoveNPC;
import de.markusbordihn.easynpc.network.message.MessageRespawnNPC;
import de.markusbordihn.easynpc.network.message.MessageSaveDialog;
import de.markusbordihn.easynpc.network.message.MessageSaveDialogButton;
import de.markusbordihn.easynpc.network.message.MessageSaveDialogSet;
import de.markusbordihn.easynpc.network.message.MessageScaleChange;
import de.markusbordihn.easynpc.network.message.MessageSkinChange;
import de.markusbordihn.easynpc.network.message.MessageTradingTypeChange;
import de.markusbordihn.easynpc.network.message.MessageTriggerActionEvent;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NetworkMessageHandler implements NetworkMessageHandlerInterface {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public NetworkMessageHandler() {
    // Register network message handler.
  }

  public static void changeSpawnerSettings(
      BlockPos blockPos, SpawnerSettingType spawnerSettingType, int value) {
    if (blockPos != null && spawnerSettingType != null) {
      NetworkHandler.sendToServer(
          new ChangeSpawnerSettingMessage(blockPos, spawnerSettingType, value));
    }
  }

  /** Send action change. */
  public static void actionEventChange(
      UUID uuid, ActionEventType actionEventType, ActionDataEntry actionDataEntry) {
    if (uuid != null
        && actionEventType != null
        && actionDataEntry != null
        && actionDataEntry.isValid()) {
      NetworkHandler.sendToServer(
          new MessageActionEventChange(uuid, actionEventType, actionDataEntry));
    }
  }

  /** Send name change. */
  public static void nameChange(UUID uuid, String name, int color) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandler.sendToServer(new MessageNameChange(uuid, name, color));
    }
  }

  /** Open configuration request. */
  public static void openConfiguration(UUID uuid, ConfigurationType configurationType) {
    if (uuid != null && configurationType != null) {
      NetworkHandler.sendToServer(new MessageOpenConfiguration(uuid, configurationType, 0));
    }
  }

  public static void openConfiguration(
      UUID uuid, ConfigurationType configurationType, int pageIndex) {
    if (uuid != null && configurationType != null && pageIndex >= 0) {
      NetworkHandler.sendToServer(new MessageOpenConfiguration(uuid, configurationType, pageIndex));
    }
  }

  /** Open dialog editor request */
  public static void openDialogEditor(
      UUID uuid, UUID dialogId, ConfigurationType formerConfigurationType) {
    if (uuid != null && dialogId != null) {
      NetworkHandler.sendToServer(
          new MessageOpenDialogEditor(uuid, dialogId, formerConfigurationType));
    }
  }

  public static void openDialogEditor(UUID uuid, ConfigurationType formerConfigurationType) {
    if (uuid != null) {
      NetworkHandler.sendToServer(
          new MessageOpenDialogEditor(uuid, new UUID(0L, 0L), formerConfigurationType));
    }
  }

  /** Open dialog button editor request */
  public static void openDialogButtonEditor(
      UUID uuid, UUID dialogId, ConfigurationType formerConfigurationType) {
    if (uuid != null && dialogId != null) {
      NetworkHandler.sendToServer(
          new MessageOpenDialogButtonEditor(
              uuid, dialogId, new UUID(0L, 0L), formerConfigurationType));
    }
  }

  public static void openDialogButtonEditor(
      UUID uuid, UUID dialogId, UUID dialogButtonId, ConfigurationType formerConfigurationType) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandler.sendToServer(
          new MessageOpenDialogButtonEditor(
              uuid, dialogId, dialogButtonId, formerConfigurationType));
    }
  }

  public static void openDialogTextEditor(
      UUID uuid, UUID dialogId, ConfigurationType formerConfigurationType) {
    if (uuid != null && dialogId != null) {
      NetworkHandler.sendToServer(
          new MessageOpenDialogTextEditor(uuid, dialogId, formerConfigurationType));
    }
  }

  /** Open dialog screen. */
  public static void openDialog(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandler.sendToServer(new MessageOpenDialog(uuid, dialogId, 0));
    }
  }

  public static void openDialog(UUID uuid, UUID dialogId, int pageIndex) {
    if (uuid != null && dialogId != null && pageIndex >= 0) {
      NetworkHandler.sendToServer(new MessageOpenDialog(uuid, dialogId, pageIndex));
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
  public static void modelVisibilityChange(
      UUID uuid, EquipmentSlot equipmentSlot, boolean visible) {
    if (uuid != null && equipmentSlot != null) {
      NetworkHandler.sendToServer(
          new MessageModelEquipmentVisibilityChange(uuid, equipmentSlot, visible));
    }
  }

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

  /** Send respawn NPC. */
  public static void respawnNPC(UUID uuid) {
    if (uuid != null) {
      NetworkHandler.sendToServer(new MessageRespawnNPC(uuid));
    }
  }

  /** Send rotation change. */
  public static void rotationChange(UUID uuid, ModelPart modelPart, CustomRotation rotation) {
    if (uuid != null && modelPart != null && rotation != null) {
      NetworkHandler.sendToServer(new MessageModelRotationChange(uuid, modelPart, rotation));
    }
  }

  /** Save dialog. */
  public static void saveDialog(UUID uuid, DialogDataSet dialogDataSet) {
    if (uuid != null && dialogDataSet != null) {
      NetworkHandler.sendToServer(new MessageSaveDialogSet(uuid, dialogDataSet));
    }
  }

  public static void saveDialog(UUID uuid, UUID dialogId, DialogDataEntry dialogData) {
    if (uuid != null && dialogId != null && dialogData != null) {
      NetworkHandler.sendToServer(new MessageSaveDialog(uuid, dialogId, dialogData));
    }
  }

  /** Send scale change. */
  public static void scaleChange(UUID uuid, String scaleAxis, float scale) {
    if (uuid != null && scaleAxis != null) {
      NetworkHandler.sendToServer(new MessageScaleChange(uuid, scaleAxis, scale));
    }
  }

  /** Send skin change. */
  public static void setNoneSkin(UUID uuid) {
    if (uuid != null) {
      NetworkHandler.sendToServer(
          new MessageSkinChange(uuid, "", "", Constants.BLANK_UUID, SkinType.NONE, ""));
    }
  }

  public static void setDefaultSkin(UUID uuid, Enum<?> variant) {
    if (uuid != null && variant != null) {
      NetworkHandler.sendToServer(
          new MessageSkinChange(
              uuid, "", "", Constants.BLANK_UUID, SkinType.DEFAULT, variant.name()));
    }
  }

  public static void setCustomSkin(UUID uuid, UUID skinUUID) {
    if (uuid != null && skinUUID != null) {
      NetworkHandler.sendToServer(
          new MessageSkinChange(uuid, "", "", skinUUID, SkinType.CUSTOM, ""));
    }
  }

  public static void setPlayerSkin(UUID uuid, String playerName, UUID playerUUID) {
    if (uuid != null && playerName != null && playerUUID != null) {
      NetworkHandler.sendToServer(
          new MessageSkinChange(uuid, playerName, "", playerUUID, SkinType.PLAYER_SKIN, ""));
    }
  }

  public static void setRemoteSkin(UUID uuid, String skinURL) {
    if (uuid != null && UrlValidator.isValidUrl(skinURL)) {
      NetworkHandler.sendToServer(
          new MessageSkinChange(
              uuid, "", skinURL, Constants.BLANK_UUID, SkinType.INSECURE_REMOTE_URL, ""));
    }
  }

  public static void skinChange(
      UUID uuid, String skin, String skinURL, UUID skinUUID, SkinType skinType) {
    if (uuid != null && skin != null && skinType != null) {
      NetworkHandler.sendToServer(
          new MessageSkinChange(uuid, skin, skinURL, skinUUID, skinType, ""));
    }
  }

  /** Send trigger action. */
  public static void triggerActionEvent(UUID uuid, ActionEventType actionEventType) {
    if (uuid != null && actionEventType != null && actionEventType != ActionEventType.NONE) {
      NetworkHandler.sendToServer(new MessageTriggerActionEvent(uuid, actionEventType));
    }
  }

  /** Handle Dialog Button actions. */
  public static void triggerDialogButtonAction(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandler.sendToServer(new MessageDialogButtonAction(uuid, dialogId, dialogButtonId));
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
  public static void entityAttributeChange(
      UUID uuid, EntityAttribute entityAttribute, Boolean booleanValue) {
    if (uuid != null && entityAttribute != null && booleanValue != null) {
      NetworkHandler.sendToServer(
          new MessageEntityAttributeChange(uuid, entityAttribute, booleanValue));
    }
  }

  public static void entityAttributeChange(
      UUID uuid, EntityAttribute entityAttribute, Float floatValue) {
    if (uuid != null && entityAttribute != null && floatValue != null) {
      NetworkHandler.sendToServer(
          new MessageEntityAttributeChange(uuid, entityAttribute, floatValue));
    }
  }

  public static void entityAttributeChange(
      UUID uuid, EntityAttribute entityAttribute, Integer integerValue) {
    if (uuid != null && entityAttribute != null && integerValue != null) {
      NetworkHandler.sendToServer(
          new MessageEntityAttributeChange(uuid, entityAttribute, integerValue));
    }
  }

  public static void entityBaseAttributeChange(UUID uuid, Attribute attribute, Double value) {
    if (uuid != null && attribute != null && attribute.getRegistryName() != null && value != null) {
      Double roundedValue = Math.round(value * 100.0) / 100.0;
      NetworkHandler.sendToServer(
          new MessageEntityBaseAttributeChange(uuid, attribute.getRegistryName(), roundedValue));
    }
  }

  public static void entityAttributeChange(
      UUID uuid, EntityAttribute entityAttribute, String stringValue) {
    if (uuid != null && entityAttribute != null && stringValue != null) {
      NetworkHandler.sendToServer(
          new MessageEntityAttributeChange(uuid, entityAttribute, stringValue));
    }
  }

  /** Change trading type. */
  public static void changeTradingType(UUID uuid, TradingType tradingType) {
    if (uuid != null && tradingType != null) {
      NetworkHandler.sendToServer(new MessageTradingTypeChange(uuid, tradingType));
    }
  }

  public static void setAdvancedTradingResetsEveryMin(UUID uuid, int resetsEveryMin) {
    if (uuid != null && resetsEveryMin >= 0) {
      NetworkHandler.sendToServer(
          new MessageAdvancedTrading(
              uuid,
              9999,
              MessageAdvancedTrading.TradingValueType.RESETS_EVERY_MIN,
              resetsEveryMin));
    }
  }

  public static void setAdvancedTradingMaxUses(UUID uuid, int tradingOfferIndex, int maxUses) {
    if (uuid != null && maxUses > 0) {
      NetworkHandler.sendToServer(
          new MessageAdvancedTrading(
              uuid, tradingOfferIndex, MessageAdvancedTrading.TradingValueType.MAX_USES, maxUses));
    }
  }

  public static void setAdvancedTradingRewardExp(UUID uuid, int tradingOfferIndex, int xp) {
    if (uuid != null && xp >= 0) {
      NetworkHandler.sendToServer(
          new MessageAdvancedTrading(
              uuid, tradingOfferIndex, MessageAdvancedTrading.TradingValueType.XP, xp));
    }
  }

  public static void setAdvancedTradingPriceMultiplier(
      UUID uuid, int tradingOfferIndex, float priceMultiplier) {
    if (uuid != null && priceMultiplier >= 0.0) {
      NetworkHandler.sendToServer(
          new MessageAdvancedTrading(
              uuid,
              tradingOfferIndex,
              MessageAdvancedTrading.TradingValueType.PRICE_MULTIPLIER,
              priceMultiplier));
    }
  }

  public static void setAdvancedTradingDemand(UUID uuid, int tradingOfferIndex, int demand) {
    if (uuid != null && demand >= 0) {
      NetworkHandler.sendToServer(
          new MessageAdvancedTrading(
              uuid, tradingOfferIndex, MessageAdvancedTrading.TradingValueType.DEMAND, demand));
    }
  }

  public static void setBasicTradingMaxUses(UUID uuid, int maxUses) {
    if (uuid != null && maxUses > 0) {
      NetworkHandler.sendToServer(
          new MessageBasicTrading(uuid, MessageBasicTrading.TradingValueType.MAX_USES, maxUses));
    }
  }

  public static void setBasicTradingRewardExp(UUID uuid, int rewardExp) {
    if (uuid != null && rewardExp >= 0) {
      NetworkHandler.sendToServer(
          new MessageBasicTrading(
              uuid, MessageBasicTrading.TradingValueType.REWARD_EXP, rewardExp));
    }
  }

  public static void setBasicTradingResetsEveryMin(UUID uuid, int resetsEveryMin) {
    if (uuid != null && resetsEveryMin >= 0) {
      NetworkHandler.sendToServer(
          new MessageBasicTrading(
              uuid, MessageBasicTrading.TradingValueType.RESETS_EVERY_MIN, resetsEveryMin));
    }
  }

  public static void addObjective(UUID uuid, ObjectiveDataEntry objectiveDataEntry) {
    if (uuid != null && objectiveDataEntry != null) {
      NetworkHandler.sendToServer(new MessageObjectiveAdd(uuid, objectiveDataEntry));
    }
  }

  public static void removeObjective(UUID uuid, ObjectiveDataEntry objectiveDataEntry) {
    if (uuid != null && objectiveDataEntry != null) {
      NetworkHandler.sendToServer(new MessageObjectiveRemove(uuid, objectiveDataEntry));
    }
  }

  public static void saveDialogButton(
      UUID uuid, UUID dialogId, UUID dialogButtonId, DialogButtonData dialogButtonData) {
    if (uuid != null && dialogId != null && dialogButtonId != null && dialogButtonData != null) {
      NetworkHandler.sendToServer(
          new MessageSaveDialogButton(uuid, dialogId, dialogButtonId, dialogButtonData));
    }
  }

  public static void removeDialog(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandler.sendToServer(new MessageRemoveDialog(uuid, dialogId));
    }
  }

  public static void removeDialogButton(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandler.sendToServer(new MessageRemoveDialogButton(uuid, dialogId, dialogButtonId));
    }
  }

  /** Export preset to player */
  public void exportPresetClient(UUID uuid, String name, ServerPlayer serverPlayer) {
    if (uuid == null
        || name == null
        || name.isEmpty()
        || serverPlayer == null
        || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    PresetData<?> presetData = easyNPC.getEasyNPCPresetData();
    CompoundTag compoundTag = presetData.exportPresetData();
    log.info(
        "Exporting preset for {} to {}",
        easyNPC.getEntity().getName().getString(),
        serverPlayer.getName().getString());
    NetworkHandler.sendToPlayer(
        new MessagePresetExportClient(
            uuid,
            easyNPC.getEntity().getName().getString(),
            easyNPC.getEasyNPCSkinData().getSkinModel(),
            name,
            compoundTag),
        serverPlayer);
  }
}
