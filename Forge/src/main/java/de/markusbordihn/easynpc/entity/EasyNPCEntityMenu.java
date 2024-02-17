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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.CommonConfig;
import de.markusbordihn.easynpc.data.WorldPresetData;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.action.DialogActionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.action.DistanceActionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.attribute.AbilitiesAttributeConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.attribute.BaseAttributeConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.attribute.DisplayAttributeConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.AdvancedDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.NoneDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.YesNoDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.main.MainConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.objective.AttackObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.objective.BasicObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.objective.FollowObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.objective.LookObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.AdvancedPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.DefaultPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.position.DefaultPositionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.CustomExportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.CustomImportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.DefaultImportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.WorldExportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.WorldImportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.rotation.DefaultRotationConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.scaling.ScalingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.CustomSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.DefaultSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.NoneSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.PlayerSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.UrlSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.AdvancedTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.CustomTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.NoneTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.menu.editor.DialogButtonEditorMenu;
import de.markusbordihn.easynpc.menu.editor.DialogEditorMenu;
import de.markusbordihn.easynpc.menu.editor.DialogTextEditorMenu;
import java.util.List;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCEntityMenu {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Config values
  protected static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  protected EasyNPCEntityMenu() {}

  public static void openDialogMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity, UUID dialogId, int pageIndex) {
    UUID uuid = entity.getUUID();
    ActionEventSet actionDataSet = entity.getActionEventSet();
    DialogDataSet dialogDataSet = entity.getDialogDataSet();
    serverPlayer.openMenu(
        DialogMenu.getMenuProvider(uuid, entity, actionDataSet, dialogDataSet, dialogId, pageIndex),
        buffer -> {
          buffer.writeUUID(uuid);
          buffer.writeNbt(actionDataSet.createTag());
          buffer.writeNbt(dialogDataSet.createTag());
          buffer.writeUUID(dialogId);
          buffer.writeInt(pageIndex);
        });
  }

  public static void openDialogEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPCEntity entity,
      UUID dialogId,
      ConfigurationType formerConfigurationType,
      int pageIndex) {
    UUID uuid = entity.getUUID();
    DialogDataSet dialogDataSet = entity.getDialogDataSet();
    serverPlayer.openMenu(
        DialogEditorMenu.getMenuProvider(
            uuid, entity, dialogDataSet, dialogId, formerConfigurationType, pageIndex),
        buffer -> {
          buffer.writeUUID(uuid);
          buffer.writeNbt(dialogDataSet.createTag());
          buffer.writeUUID(dialogId);
          buffer.writeEnum(formerConfigurationType);
          buffer.writeInt(pageIndex);
        });
  }

  public static void openDialogButtonEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPCEntity entity,
      UUID dialogId,
      UUID dialogButtonId,
      ConfigurationType formerConfigurationType,
      int pageIndex) {
    UUID uuid = entity.getUUID();
    DialogDataSet dialogDataSet = entity.getDialogDataSet();
    serverPlayer.openMenu(
        DialogButtonEditorMenu.getMenuProvider(
            uuid,
            entity,
            dialogDataSet,
            dialogId,
            dialogButtonId,
            formerConfigurationType,
            pageIndex),
        buffer -> {
          buffer.writeUUID(uuid);
          buffer.writeNbt(dialogDataSet.createTag());
          buffer.writeUUID(dialogId);
          buffer.writeUUID(dialogButtonId);
          buffer.writeEnum(formerConfigurationType);
          buffer.writeInt(pageIndex);
        });
  }

  public static void openDialogTextEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPCEntity entity,
      UUID dialogId,
      ConfigurationType formerConfigurationType,
      int pageIndex) {
    UUID uuid = entity.getUUID();
    DialogDataSet dialogDataSet = entity.getDialogDataSet();
    serverPlayer.openMenu(
        DialogTextEditorMenu.getMenuProvider(
            uuid, entity, dialogDataSet, dialogId, formerConfigurationType, pageIndex),
        buffer -> {
          buffer.writeUUID(uuid);
          buffer.writeNbt(dialogDataSet.createTag());
          buffer.writeUUID(dialogId);
          buffer.writeEnum(formerConfigurationType);
          buffer.writeInt(pageIndex);
        });
  }

  public static void openEquipmentConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.equipmentConfigurationEnabled.get(),
        COMMON.equipmentConfigurationAllowInCreative.get(),
        COMMON.equipmentConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          EquipmentConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBasicActionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.basicActionConfigurationEnabled.get(),
        COMMON.basicActionConfigurationAllowInCreative.get(),
        COMMON.basicActionConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      ActionEventSet actionDataSet = entity.getActionEventSet();
      serverPlayer.openMenu(
          BasicActionConfigurationMenu.getMenuProvider(uuid, entity, actionDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(actionDataSet.createTag());
          });
    }
  }

  public static void openDialogActionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.dialogActionConfigurationEnabled.get(),
        COMMON.dialogActionConfigurationAllowInCreative.get(),
        COMMON.dialogActionConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      ActionEventSet actionDataSet = entity.getActionEventSet();
      serverPlayer.openMenu(
          DialogActionConfigurationMenu.getMenuProvider(uuid, entity, actionDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(actionDataSet.createTag());
          });
    }
  }

  public static void openDistanceActionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.distanceActionConfigurationEnabled.get(),
        COMMON.distanceActionConfigurationAllowInCreative.get(),
        COMMON.distanceActionConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      ActionEventSet actionDataSet = entity.getActionEventSet();
      serverPlayer.openMenu(
          DistanceActionConfigurationMenu.getMenuProvider(uuid, entity, actionDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(actionDataSet.createTag());
          });
    }
  }

  public static void openBasicDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.basicDialogConfigurationEnabled.get(),
        COMMON.basicDialogConfigurationAllowInCreative.get(),
        COMMON.basicDialogConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      DialogDataSet dialogDataSet = entity.getDialogDataSet();
      serverPlayer.openMenu(
          BasicDialogConfigurationMenu.getMenuProvider(uuid, entity, dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openYesNoDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.yesNoDialogConfigurationEnabled.get(),
        COMMON.yesNoDialogConfigurationAllowInCreative.get(),
        COMMON.yesNoDialogConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      DialogDataSet dialogDataSet = entity.getDialogDataSet();
      serverPlayer.openMenu(
          YesNoDialogConfigurationMenu.getMenuProvider(uuid, entity, dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openAdvancedDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.advancedDialogConfigurationEnabled.get(),
        COMMON.advancedDialogConfigurationAllowInCreative.get(),
        COMMON.advancedDialogConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      DialogDataSet dialogDataSet = entity.getDialogDataSet();
      serverPlayer.openMenu(
          AdvancedDialogConfigurationMenu.getMenuProvider(uuid, entity, dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openMainConfigurationMenu(ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.mainConfigurationEnabled.get(),
        COMMON.mainConfigurationAllowInCreative.get(),
        COMMON.mainConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      DialogType dialogType = entity.getDialogDataSet().getType();
      serverPlayer.openMenu(
          MainConfigurationMenu.getMenuProvider(uuid, entity, dialogType),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeEnum(dialogType);
          });
    }
  }

  public static void openAdvancedPoseConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.advancedPoseConfigurationEnabled.get(),
        COMMON.advancedPoseConfigurationAllowInCreative.get(),
        COMMON.advancedPoseConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          AdvancedPoseConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openCustomPoseConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.customPoseConfigurationEnabled.get(),
        COMMON.customPoseConfigurationAllowInCreative.get(),
        COMMON.customPoseConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          CustomPoseConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultPoseConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.defaultPoseConfigurationEnabled.get(),
        COMMON.defaultPoseConfigurationAllowInCreative.get(),
        COMMON.defaultPoseConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          DefaultPoseConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultPositionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.defaultPositionConfigurationEnabled.get(),
        COMMON.defaultPositionConfigurationAllowInCreative.get(),
        COMMON.defaultPositionConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          DefaultPositionConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultRotationConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.defaultRotationConfigurationEnabled.get(),
        COMMON.defaultRotationConfigurationAllowInCreative.get(),
        COMMON.defaultRotationConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          DefaultRotationConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openCustomSkinConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.customSkinConfigurationEnabled.get(),
        COMMON.customSkinConfigurationAllowInCreative.get(),
        COMMON.customSkinConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          CustomSkinConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultSkinConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.defaultSkinConfigurationEnabled.get(),
        COMMON.defaultSkinConfigurationAllowInCreative.get(),
        COMMON.defaultSkinConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          DefaultSkinConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openNoneSkinConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.noneSkinConfigurationEnabled.get(),
        COMMON.noneSkinConfigurationAllowInCreative.get(),
        COMMON.noneSkinConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          NoneSkinConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openNoneDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.noneDialogConfigurationEnabled.get(),
        COMMON.noneDialogConfigurationAllowInCreative.get(),
        COMMON.noneDialogConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      DialogDataSet dialogDataSet = entity.getDialogDataSet();
      serverPlayer.openMenu(
          NoneDialogConfigurationMenu.getMenuProvider(uuid, entity, dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openPlayerSkinConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.playerSkinConfigurationEnabled.get(),
        COMMON.playerSkinConfigurationAllowInCreative.get(),
        COMMON.playerSkinConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          PlayerSkinConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openUrlSkinConfigurationMenu(ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.urlSkinConfigurationEnabled.get(),
        COMMON.urlSkinConfigurationAllowInCreative.get(),
        COMMON.urlSkinConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          UrlSkinConfigurationMenu.getMenuProvider(uuid, entity), buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openScalingConfigurationMenu(ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.scalingConfigurationEnabled.get(),
        COMMON.scalingConfigurationAllowInCreative.get(),
        COMMON.scalingConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          ScalingConfigurationMenu.getMenuProvider(uuid, entity), buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openCustomPresetExportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity easyNPCEntity) {
    if (hasPermissions(
        serverPlayer,
        easyNPCEntity,
        COMMON.customExportPresetConfigurationEnabled.get(),
        COMMON.customExportPresetConfigurationAllowInCreative.get(),
        COMMON.customExportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPCEntity.getUUID();
      serverPlayer.openMenu(
          CustomExportPresetConfigurationMenu.getMenuProvider(uuid, easyNPCEntity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openWorldPresetExportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity easyNPCEntity) {
    if (hasPermissions(
        serverPlayer,
        easyNPCEntity,
        COMMON.worldExportPresetConfigurationEnabled.get(),
        COMMON.worldExportPresetConfigurationAllowInCreative.get(),
        COMMON.worldExportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPCEntity.getUUID();
      serverPlayer.openMenu(
          WorldExportPresetConfigurationMenu.getMenuProvider(uuid, easyNPCEntity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultPresetImportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity easyNPCEntity) {
    if (hasPermissions(
        serverPlayer,
        easyNPCEntity,
        COMMON.defaultImportPresetConfigurationEnabled.get(),
        COMMON.defaultImportPresetConfigurationAllowInCreative.get(),
        COMMON.defaultImportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPCEntity.getUUID();
      serverPlayer.openMenu(
          DefaultImportPresetConfigurationMenu.getMenuProvider(uuid, easyNPCEntity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openServerPresetImportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity easyNPCEntity) {
    if (hasPermissions(
        serverPlayer,
        easyNPCEntity,
        COMMON.worldImportPresetConfigurationEnabled.get(),
        COMMON.worldImportPresetConfigurationAllowInCreative.get(),
        COMMON.worldImportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPCEntity.getUUID();
      List<ResourceLocation> worldPresets =
          WorldPresetData.getPresetFilePathResourceLocations().toList();
      serverPlayer.openMenu(
          WorldImportPresetConfigurationMenu.getMenuProvider(uuid, easyNPCEntity, worldPresets),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeVarInt(worldPresets.size());
            for (ResourceLocation worldPreset : worldPresets) {
              buffer.writeResourceLocation(worldPreset);
            }
          });
    }
  }

  public static void openCustomPresetImportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity easyNPCEntity) {
    if (hasPermissions(
        serverPlayer,
        easyNPCEntity,
        COMMON.customImportPresetConfigurationEnabled.get(),
        COMMON.customImportPresetConfigurationAllowInCreative.get(),
        COMMON.customImportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPCEntity.getUUID();
      serverPlayer.openMenu(
          CustomImportPresetConfigurationMenu.getMenuProvider(uuid, easyNPCEntity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openNoneTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.noneTradingConfigurationEnabled.get(),
        COMMON.noneTradingConfigurationAllowInCreative.get(),
        COMMON.noneTradingConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          NoneTradingConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBasicTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.basicTradingConfigurationEnabled.get(),
        COMMON.basicTradingConfigurationAllowInCreative.get(),
        COMMON.basicTradingConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          BasicTradingConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openAdvancedTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity, int pageIndex) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.advancedTradingConfigurationEnabled.get(),
        COMMON.advancedTradingConfigurationAllowInCreative.get(),
        COMMON.advancedTradingConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          AdvancedTradingConfigurationMenu.getMenuProvider(uuid, entity, pageIndex),
          buffer -> buffer.writeUUID(uuid).writeInt(pageIndex));
    }
  }

  public static void openCustomTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.customTradingConfigurationEnabled.get(),
        COMMON.customTradingConfigurationAllowInCreative.get(),
        COMMON.customTradingConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          CustomTradingConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBasicObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.basicObjectiveConfigurationEnabled.get(),
        COMMON.basicObjectiveConfigurationAllowInCreative.get(),
        COMMON.basicObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      ObjectiveDataSet objectiveDataSet = entity.getObjectiveDataSet();
      serverPlayer.openMenu(
          BasicObjectiveConfigurationMenu.getMenuProvider(uuid, entity, objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openAttackObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.attackObjectiveConfigurationEnabled.get(),
        COMMON.attackObjectiveConfigurationAllowInCreative.get(),
        COMMON.attackObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      ObjectiveDataSet objectiveDataSet = entity.getObjectiveDataSet();
      serverPlayer.openMenu(
          AttackObjectiveConfigurationMenu.getMenuProvider(uuid, entity, objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openFollowObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.followObjectiveConfigurationEnabled.get(),
        COMMON.followObjectiveConfigurationAllowInCreative.get(),
        COMMON.followObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      ObjectiveDataSet objectiveDataSet = entity.getObjectiveDataSet();
      serverPlayer.openMenu(
          FollowObjectiveConfigurationMenu.getMenuProvider(uuid, entity, objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openLookObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.lookObjectiveConfigurationEnabled.get(),
        COMMON.lookObjectiveConfigurationAllowInCreative.get(),
        COMMON.lookObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      ObjectiveDataSet objectiveDataSet = entity.getObjectiveDataSet();
      serverPlayer.openMenu(
          LookObjectiveConfigurationMenu.getMenuProvider(uuid, entity, objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openAbilitiesAttributeConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.abilitiesAttributeConfigurationEnabled.get(),
        COMMON.abilitiesAttributeConfigurationAllowInCreative.get(),
        COMMON.abilitiesAttributeConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          AbilitiesAttributeConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBaseAttributeConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.baseAttributeConfigurationEnabled.get(),
        COMMON.baseAttributeConfigurationAllowInCreative.get(),
        COMMON.baseAttributeConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          BaseAttributeConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeDouble(
                entity.getAttribute(Attributes.FOLLOW_RANGE) != null
                    ? entity.getAttribute(Attributes.FOLLOW_RANGE).getBaseValue()
                    : 32.0d);
            buffer.writeDouble(
                entity.getAttribute(Attributes.KNOCKBACK_RESISTANCE) != null
                    ? entity.getAttribute(Attributes.KNOCKBACK_RESISTANCE).getBaseValue()
                    : 0.0d);
            buffer.writeDouble(
                entity.getAttribute(Attributes.ATTACK_DAMAGE) != null
                    ? entity.getAttribute(Attributes.ATTACK_DAMAGE).getBaseValue()
                    : 2.0d);
            buffer.writeDouble(
                entity.getAttribute(Attributes.ATTACK_KNOCKBACK) != null
                    ? entity.getAttribute(Attributes.ATTACK_KNOCKBACK).getBaseValue()
                    : 0.0d);
          });
    }
  }

  public static void openDisplayAttributeConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPCEntity entity) {
    if (hasPermissions(
        serverPlayer,
        entity,
        COMMON.displayAttributeConfigurationEnabled.get(),
        COMMON.displayAttributeConfigurationAllowInCreative.get(),
        COMMON.displayAttributeConfigurationPermissionLevel.get())) {
      UUID uuid = entity.getUUID();
      serverPlayer.openMenu(
          DisplayAttributeConfigurationMenu.getMenuProvider(uuid, entity),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  private static boolean hasPermissions(
      ServerPlayer serverPlayer,
      EasyNPCEntity entity,
      Boolean enabled,
      Boolean allowInCreative,
      int permissionLevel) {
    if (Boolean.FALSE.equals(enabled) || serverPlayer == null) {
      return false;
    } else if (Boolean.TRUE.equals(allowInCreative) && serverPlayer.isCreative()) {
      return true;
    } else if (!entity.hasOwner() || !entity.isOwner(serverPlayer)) {
      return false;
    } else {
      return serverPlayer.hasPermissions(permissionLevel);
    }
  }
}
