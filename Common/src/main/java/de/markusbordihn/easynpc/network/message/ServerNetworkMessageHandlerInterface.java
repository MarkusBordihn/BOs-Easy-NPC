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

package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.spawner.SpawnerSettingType;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.message.server.AddObjectiveMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeActionEventMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeAdvancedTradingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeBasicTradingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeEntityAttributeMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeEntityBaseAttributeMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelEquipmentVisibilityMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelLockRotationMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelPositionMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelRotationMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeModelVisibilityMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeNameMessage;
import de.markusbordihn.easynpc.network.message.server.ChangePoseMessage;
import de.markusbordihn.easynpc.network.message.server.ChangePositionMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeProfessionMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeScaleMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeSkinMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeSpawnerSettingMessage;
import de.markusbordihn.easynpc.network.message.server.ChangeTradingTypeMessage;
import de.markusbordihn.easynpc.network.message.server.ExecuteActionEventMessage;
import de.markusbordihn.easynpc.network.message.server.ExecuteDialogButtonActionMessage;
import de.markusbordihn.easynpc.network.message.server.ExportPresetMessage;
import de.markusbordihn.easynpc.network.message.server.ExportWorldPresetMessage;
import de.markusbordihn.easynpc.network.message.server.ImportPresetMessage;
import de.markusbordihn.easynpc.network.message.server.OpenConfigurationMessage;
import de.markusbordihn.easynpc.network.message.server.OpenDialogButtonEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenDialogEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenDialogTextEditorMessage;
import de.markusbordihn.easynpc.network.message.server.OpenMenuMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveDialogButtonMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveDialogMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveNPCMessage;
import de.markusbordihn.easynpc.network.message.server.RemoveObjectiveMessage;
import de.markusbordihn.easynpc.network.message.server.RespawnNPCMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogButtonMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogMessage;
import de.markusbordihn.easynpc.network.message.server.SaveDialogSetMessage;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface ServerNetworkMessageHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  default void actionEventChange(
      UUID uuid, ActionEventType actionEventType, ActionDataEntry actionDataEntry) {
    if (uuid != null
        && actionEventType != null
        && actionDataEntry != null
        && actionDataEntry.isValid()) {
      NetworkHandlerManager.sendToServer(
          ChangeActionEventMessage.MESSAGE_ID,
          new ChangeActionEventMessage(uuid, actionEventType, actionDataEntry));
    }
  }

  default void addObjective(UUID uuid, ObjectiveDataEntry objectiveDataEntry) {
    if (uuid != null && objectiveDataEntry != null) {
      NetworkHandlerManager.sendToServer(
          AddObjectiveMessage.MESSAGE_ID, new AddObjectiveMessage(uuid, objectiveDataEntry));
    }
  }

  default void changeProfession(UUID uuid, Profession profession) {
    if (uuid != null && profession != null) {
      NetworkHandlerManager.sendToServer(
          ChangeProfessionMessage.MESSAGE_ID, new ChangeProfessionMessage(uuid, profession));
    }
  }

  default void changeTradingType(UUID uuid, TradingType tradingType) {
    if (uuid != null && tradingType != null) {
      NetworkHandlerManager.sendToServer(
          ChangeTradingTypeMessage.MESSAGE_ID, new ChangeTradingTypeMessage(uuid, tradingType));
    }
  }

  default void setAdvancedTradingResetsEveryMin(UUID uuid, int resetsEveryMin) {
    if (uuid != null && resetsEveryMin >= 0) {
      NetworkHandlerManager.sendToServer(
          ChangeAdvancedTradingMessage.MESSAGE_ID,
          new ChangeAdvancedTradingMessage(
              uuid,
              9999,
              ChangeAdvancedTradingMessage.TradingValueType.RESETS_EVERY_MIN,
              resetsEveryMin));
    }
  }

  default void setAdvancedTradingMaxUses(UUID uuid, int tradingOfferIndex, int maxUses) {
    if (uuid != null && maxUses > 0) {
      NetworkHandlerManager.sendToServer(
          ChangeAdvancedTradingMessage.MESSAGE_ID,
          new ChangeAdvancedTradingMessage(
              uuid,
              tradingOfferIndex,
              ChangeAdvancedTradingMessage.TradingValueType.MAX_USES,
              maxUses));
    }
  }

  default void setAdvancedTradingRewardExp(UUID uuid, int tradingOfferIndex, int xp) {
    if (uuid != null && xp >= 0) {
      NetworkHandlerManager.sendToServer(
          ChangeAdvancedTradingMessage.MESSAGE_ID,
          new ChangeAdvancedTradingMessage(
              uuid, tradingOfferIndex, ChangeAdvancedTradingMessage.TradingValueType.XP, xp));
    }
  }

  default void setAdvancedTradingPriceMultiplier(
      UUID uuid, int tradingOfferIndex, float priceMultiplier) {
    if (uuid != null && priceMultiplier >= 0.0) {
      NetworkHandlerManager.sendToServer(
          ChangeAdvancedTradingMessage.MESSAGE_ID,
          new ChangeAdvancedTradingMessage(
              uuid,
              tradingOfferIndex,
              ChangeAdvancedTradingMessage.TradingValueType.PRICE_MULTIPLIER,
              priceMultiplier));
    }
  }

  default void setAdvancedTradingDemand(UUID uuid, int tradingOfferIndex, int demand) {
    if (uuid != null && demand >= 0) {
      NetworkHandlerManager.sendToServer(
          ChangeAdvancedTradingMessage.MESSAGE_ID,
          new ChangeAdvancedTradingMessage(
              uuid,
              tradingOfferIndex,
              ChangeAdvancedTradingMessage.TradingValueType.DEMAND,
              demand));
    }
  }

  default void entityAttributeChange(
      UUID uuid, EntityAttribute entityAttribute, Boolean booleanValue) {
    if (uuid != null && entityAttribute != null && booleanValue != null) {
      NetworkHandlerManager.sendToServer(
          ChangeEntityAttributeMessage.MESSAGE_ID,
          new ChangeEntityAttributeMessage(uuid, entityAttribute, booleanValue));
    }
  }

  default void entityAttributeChange(
      UUID uuid, EntityAttribute entityAttribute, Integer integerValue) {
    if (uuid != null && entityAttribute != null && integerValue != null) {
      NetworkHandlerManager.sendToServer(
          ChangeEntityAttributeMessage.MESSAGE_ID,
          new ChangeEntityAttributeMessage(uuid, entityAttribute, integerValue));
    }
  }

  default void setBasicTradingMaxUses(UUID uuid, int maxUses) {
    if (uuid != null && maxUses > 0) {
      NetworkHandlerManager.sendToServer(
          ChangeBasicTradingMessage.MESSAGE_ID,
          new ChangeBasicTradingMessage(
              uuid, ChangeBasicTradingMessage.TradingValueType.MAX_USES, maxUses));
    }
  }

  default void setBasicTradingRewardExp(UUID uuid, int rewardExp) {
    if (uuid != null && rewardExp >= 0) {
      NetworkHandlerManager.sendToServer(
          ChangeBasicTradingMessage.MESSAGE_ID,
          new ChangeBasicTradingMessage(
              uuid, ChangeBasicTradingMessage.TradingValueType.REWARD_EXP, rewardExp));
    }
  }

  default void setBasicTradingResetsEveryMin(UUID uuid, int resetsEveryMin) {
    if (uuid != null && resetsEveryMin >= 0) {
      NetworkHandlerManager.sendToServer(
          ChangeBasicTradingMessage.MESSAGE_ID,
          new ChangeBasicTradingMessage(
              uuid, ChangeBasicTradingMessage.TradingValueType.RESETS_EVERY_MIN, resetsEveryMin));
    }
  }

  default void openDialogEditor(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendToServer(
          OpenDialogEditorMessage.MESSAGE_ID, new OpenDialogEditorMessage(uuid, dialogId));
    }
  }

  default void openDialogTextEditor(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendToServer(
          OpenDialogTextEditorMessage.MESSAGE_ID, new OpenDialogTextEditorMessage(uuid, dialogId));
    }
  }

  default void openDialogButtonEditor(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandlerManager.sendToServer(
          OpenDialogButtonEditorMessage.MESSAGE_ID,
          new OpenDialogButtonEditorMessage(uuid, dialogId, dialogButtonId));
    }
  }

  default void openMenu(UUID uuid, UUID menuId) {
    if (uuid != null && menuId != null) {
      NetworkHandlerManager.sendToServer(
          OpenMenuMessage.MESSAGE_ID, new OpenMenuMessage(uuid, menuId));
    }
  }

  default void saveDialogButton(
      UUID uuid, UUID dialogId, UUID dialogButtonId, DialogButtonData dialogButtonData) {
    if (uuid != null && dialogId != null && dialogButtonId != null && dialogButtonData != null) {
      NetworkHandlerManager.sendToServer(
          SaveDialogButtonMessage.MESSAGE_ID,
          new SaveDialogButtonMessage(uuid, dialogId, dialogButtonId, dialogButtonData));
    }
  }

  default void removeDialog(UUID uuid, UUID dialogId) {
    if (uuid != null && dialogId != null) {
      NetworkHandlerManager.sendToServer(
          RemoveDialogMessage.MESSAGE_ID, new RemoveDialogMessage(uuid, dialogId));
    }
  }

  default void removeDialogButton(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandlerManager.sendToServer(
          RemoveDialogMessage.MESSAGE_ID,
          new RemoveDialogButtonMessage(uuid, dialogId, dialogButtonId));
    }
  }

  default void saveDialogSet(UUID uuid, DialogDataSet dialogDataSet) {
    if (uuid != null && dialogDataSet != null) {
      NetworkHandlerManager.sendToServer(
          SaveDialogSetMessage.MESSAGE_ID, new SaveDialogSetMessage(uuid, dialogDataSet));
    }
  }

  default void saveDialog(UUID uuid, UUID dialogId, DialogDataEntry dialogData) {
    if (uuid != null && dialogId != null && dialogData != null) {
      NetworkHandlerManager.sendToServer(
          SaveDialogMessage.MESSAGE_ID, new SaveDialogMessage(uuid, dialogId, dialogData));
    }
  }

  default void openConfiguration(UUID uuid, ConfigurationType configurationType, int pageIndex) {
    if (uuid != null && configurationType != null && pageIndex >= 0) {
      NetworkHandlerManager.sendToServer(
          OpenConfigurationMessage.MESSAGE_ID,
          new OpenConfigurationMessage(uuid, configurationType, pageIndex));
    }
  }

  default void importPreset(UUID uuid, PresetType presetType, ResourceLocation resourceLocation) {
    if (uuid != null && presetType != null && resourceLocation != null) {
      NetworkHandlerManager.sendToServer(
          ImportPresetMessage.MESSAGE_ID,
          new ImportPresetMessage(uuid, presetType, resourceLocation));
    }
  }

  default void importPreset(UUID uuid, PresetType presetType, CompoundTag compoundTag) {
    if (uuid != null && presetType != null && compoundTag != null && !compoundTag.isEmpty()) {
      NetworkHandlerManager.sendToServer(
          ImportPresetMessage.MESSAGE_ID, new ImportPresetMessage(uuid, presetType, compoundTag));
    }
  }

  default void changeSpawnerSettings(
      BlockPos blockPos, SpawnerSettingType spawnerSettingType, int value) {
    if (blockPos != null && spawnerSettingType != null) {
      NetworkHandlerManager.sendToServer(
          ChangeSpawnerSettingMessage.MESSAGE_ID,
          new ChangeSpawnerSettingMessage(blockPos, spawnerSettingType, value));
    }
  }

  default void triggerActionEvent(UUID uuid, ActionEventType actionEventType) {
    if (uuid != null && actionEventType != null && actionEventType != ActionEventType.NONE) {
      NetworkHandlerManager.sendToServer(
          ExecuteActionEventMessage.MESSAGE_ID,
          new ExecuteActionEventMessage(uuid, actionEventType));
    }
  }

  default void triggerDialogButtonAction(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    if (uuid != null && dialogId != null && dialogButtonId != null) {
      NetworkHandlerManager.sendToServer(
          ExecuteDialogButtonActionMessage.MESSAGE_ID,
          new ExecuteDialogButtonActionMessage(uuid, dialogId, dialogButtonId));
    }
  }

  default void changeName(UUID uuid, String name, int color) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandlerManager.sendToServer(
          ChangeNameMessage.MESSAGE_ID, new ChangeNameMessage(uuid, name, color));
    }
  }

  default void removeNPC(UUID uuid) {
    if (uuid != null) {
      NetworkHandlerManager.sendToServer(RemoveNPCMessage.MESSAGE_ID, new RemoveNPCMessage(uuid));
    }
  }

  default void respawnNPC(UUID uuid) {
    if (uuid != null) {
      NetworkHandlerManager.sendToServer(RespawnNPCMessage.MESSAGE_ID, new RespawnNPCMessage(uuid));
    }
  }

  default void exportPreset(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandlerManager.sendToServer(
          ExportPresetMessage.MESSAGE_ID, new ExportPresetMessage(uuid, name));
    }
  }

  default void exportWorldPreset(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandlerManager.sendToServer(
          ExportWorldPresetMessage.MESSAGE_ID, new ExportWorldPresetMessage(uuid, name));
    }
  }

  default void setSkin(
      final UUID uuid,
      final String skinName,
      final String skinURL,
      final UUID skinUUID,
      final SkinType skinType,
      final String skinVariant) {
    if (uuid != null && skinUUID != null && skinType != null) {
      NetworkHandlerManager.sendToServer(
          ChangeSkinMessage.MESSAGE_ID,
          new ChangeSkinMessage(uuid, skinName, skinURL, skinUUID, skinType, skinVariant));
    }
  }

  default void importCustomPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.CUSTOM, resourceLocation);
  }

  default void importDataPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.DATA, resourceLocation);
  }

  default void importDefaultPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.DEFAULT, resourceLocation);
  }

  default void importLocalPreset(UUID uuid, CompoundTag compoundTag) {
    importPreset(uuid, PresetType.LOCAL, compoundTag);
  }

  default void importWorldPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.WORLD, resourceLocation);
  }

  default void openConfiguration(UUID uuid, ConfigurationType configurationType) {
    openConfiguration(uuid, configurationType, 0);
  }

  default void openDialogEditor(UUID uuid) {
    openDialogEditor(uuid, new UUID(0L, 0L));
  }

  default void openDialogButtonEditor(UUID uuid, UUID dialogId) {
    openDialogButtonEditor(uuid, dialogId, new UUID(0L, 0L));
  }

  default void setNoneSkin(final UUID uuid) {
    if (uuid != null) {
      setSkin(uuid, "", "", Constants.BLANK_UUID, SkinType.NONE, "");
    }
  }

  default void setDefaultSkin(UUID uuid, Enum<?> variant) {
    if (uuid != null && variant != null) {
      setSkin(uuid, "", "", Constants.BLANK_UUID, SkinType.DEFAULT, variant.name());
    }
  }

  default void poseChange(UUID uuid, Pose pose) {
    if (uuid != null && pose != null) {
      NetworkHandlerManager.sendToServer(
          ChangePoseMessage.MESSAGE_ID, new ChangePoseMessage(uuid, pose));
    }
  }

  default void positionChange(UUID uuid, Vec3 pos) {
    if (uuid != null && pos != null) {
      NetworkHandlerManager.sendToServer(
          ChangePositionMessage.MESSAGE_ID, new ChangePositionMessage(uuid, pos));
    }
  }

  default void entityBaseAttributeChange(UUID uuid, Attribute attribute, Double value) {
    if (uuid != null
        && attribute != null
        && value != null
        && Registry.ATTRIBUTE.getKey(attribute) != null) {
      Double roundedValue = Math.round(value * 100.0) / 100.0;
      NetworkHandlerManager.sendToServer(
          ChangeEntityBaseAttributeMessage.MESSAGE_ID,
          new ChangeEntityBaseAttributeMessage(
              uuid, Registry.ATTRIBUTE.getKey(attribute), roundedValue));
    }
  }

  default void modelLockRotationChange(UUID uuid, boolean lockRotation) {
    if (uuid != null) {
      NetworkHandlerManager.sendToServer(
          ChangeModelLockRotationMessage.MESSAGE_ID,
          new ChangeModelLockRotationMessage(uuid, lockRotation));
    }
  }

  default void modelPositionChange(UUID uuid, ModelPart modelPart, CustomPosition position) {
    if (uuid != null && modelPart != null && position != null) {
      NetworkHandlerManager.sendToServer(
          ChangeModelPositionMessage.MESSAGE_ID,
          new ChangeModelPositionMessage(uuid, modelPart, position));
    }
  }

  default void modelVisibilityChange(UUID uuid, EquipmentSlot equipmentSlot, boolean visible) {
    if (uuid != null && equipmentSlot != null) {
      NetworkHandlerManager.sendToServer(
          ChangeModelEquipmentVisibilityMessage.MESSAGE_ID,
          new ChangeModelEquipmentVisibilityMessage(uuid, equipmentSlot, visible));
    }
  }

  default void modelVisibilityChange(UUID uuid, ModelPart modelPart, boolean visible) {
    if (uuid != null && modelPart != null) {
      NetworkHandlerManager.sendToServer(
          ChangeModelVisibilityMessage.MESSAGE_ID,
          new ChangeModelVisibilityMessage(uuid, modelPart, visible));
    }
  }

  default void rotationChange(UUID uuid, ModelPart modelPart, CustomRotation rotation) {
    if (uuid != null && modelPart != null && rotation != null) {
      NetworkHandlerManager.sendToServer(
          ChangeModelRotationMessage.MESSAGE_ID,
          new ChangeModelRotationMessage(uuid, modelPart, rotation));
    }
  }

  default void scaleChange(UUID uuid, String scaleAxis, float scale) {
    if (uuid != null && scaleAxis != null) {
      NetworkHandlerManager.sendToServer(
          ChangeScaleMessage.MESSAGE_ID, new ChangeScaleMessage(uuid, scaleAxis, scale));
    }
  }

  default void setCustomSkin(UUID uuid, UUID skinUUID) {
    if (uuid != null && skinUUID != null) {
      setSkin(uuid, "", "", skinUUID, SkinType.CUSTOM, "");
    }
  }

  default void setPlayerSkin(UUID uuid, String playerName, UUID playerUUID) {
    if (uuid != null && playerName != null && playerUUID != null) {
      setSkin(uuid, playerName, "", playerUUID, SkinType.PLAYER_SKIN, "");
    }
  }

  default void setRemoteSkin(UUID uuid, String skinURL) {
    if (uuid != null && UrlValidator.isValidUrl(skinURL)) {
      setSkin(uuid, "", skinURL, Constants.BLANK_UUID, SkinType.INSECURE_REMOTE_URL, "");
    }
  }

  default void removeObjective(UUID uuid, ObjectiveDataEntry objectiveDataEntry) {
    if (uuid != null && objectiveDataEntry != null) {
      NetworkHandlerManager.sendToServer(
          RemoveObjectiveMessage.MESSAGE_ID, new RemoveObjectiveMessage(uuid, objectiveDataEntry));
    }
  }
}
