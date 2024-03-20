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

package de.markusbordihn.easynpc.menu;

import de.markusbordihn.easynpc.config.CommonConfig;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
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
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraftforge.network.NetworkHooks;

public class MenuHandler implements MenuHandlerInterface {

  protected static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  public MenuHandler() {
    // Register menu handler
  }

  public static void openEquipmentConfigurationMenu(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.equipmentConfigurationEnabled.get(),
        COMMON.equipmentConfigurationAllowInCreative.get(),
        COMMON.equipmentConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          EquipmentConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBasicActionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.basicActionConfigurationEnabled.get(),
        COMMON.basicActionConfigurationAllowInCreative.get(),
        COMMON.basicActionConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      ActionEventSet actionDataSet = easyNPC.getEasyNPCActionEventData().getActionEventSet();
      NetworkHooks.openScreen(
          serverPlayer,
          BasicActionConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), actionDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(actionDataSet.createTag());
          });
    }
  }

  public static void openDialogActionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.dialogActionConfigurationEnabled.get(),
        COMMON.dialogActionConfigurationAllowInCreative.get(),
        COMMON.dialogActionConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      ActionEventSet actionDataSet = easyNPC.getEasyNPCActionEventData().getActionEventSet();
      NetworkHooks.openScreen(
          serverPlayer,
          DialogActionConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), actionDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(actionDataSet.createTag());
          });
    }
  }

  public static void openDistanceActionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.distanceActionConfigurationEnabled.get(),
        COMMON.distanceActionConfigurationAllowInCreative.get(),
        COMMON.distanceActionConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      ActionEventSet actionDataSet = easyNPC.getEasyNPCActionEventData().getActionEventSet();
      NetworkHooks.openScreen(
          serverPlayer,
          DistanceActionConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), actionDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(actionDataSet.createTag());
          });
    }
  }

  public static void openBasicDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.basicDialogConfigurationEnabled.get(),
        COMMON.basicDialogConfigurationAllowInCreative.get(),
        COMMON.basicDialogConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          BasicDialogConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openYesNoDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.yesNoDialogConfigurationEnabled.get(),
        COMMON.yesNoDialogConfigurationAllowInCreative.get(),
        COMMON.yesNoDialogConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          YesNoDialogConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openAdvancedDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.advancedDialogConfigurationEnabled.get(),
        COMMON.advancedDialogConfigurationAllowInCreative.get(),
        COMMON.advancedDialogConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          AdvancedDialogConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openMainConfigurationMenu(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.mainConfigurationEnabled.get(),
        COMMON.mainConfigurationAllowInCreative.get(),
        COMMON.mainConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      DialogType dialogType = easyNPC.getEasyNPCDialogData().getDialogDataSet().getType();
      NetworkHooks.openScreen(
          serverPlayer,
          MainConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), dialogType),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeEnum(dialogType);
          });
    }
  }

  public static void openAdvancedPoseConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.advancedPoseConfigurationEnabled.get(),
        COMMON.advancedPoseConfigurationAllowInCreative.get(),
        COMMON.advancedPoseConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          AdvancedPoseConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openCustomPoseConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.customPoseConfigurationEnabled.get(),
        COMMON.customPoseConfigurationAllowInCreative.get(),
        COMMON.customPoseConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          CustomPoseConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultPoseConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.defaultPoseConfigurationEnabled.get(),
        COMMON.defaultPoseConfigurationAllowInCreative.get(),
        COMMON.defaultPoseConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          DefaultPoseConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultPositionConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.defaultPositionConfigurationEnabled.get(),
        COMMON.defaultPositionConfigurationAllowInCreative.get(),
        COMMON.defaultPositionConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          DefaultPositionConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultRotationConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.defaultRotationConfigurationEnabled.get(),
        COMMON.defaultRotationConfigurationAllowInCreative.get(),
        COMMON.defaultRotationConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          DefaultRotationConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openCustomSkinConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.customSkinConfigurationEnabled.get(),
        COMMON.customSkinConfigurationAllowInCreative.get(),
        COMMON.customSkinConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          CustomSkinConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultSkinConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.defaultSkinConfigurationEnabled.get(),
        COMMON.defaultSkinConfigurationAllowInCreative.get(),
        COMMON.defaultSkinConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          DefaultSkinConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openNoneSkinConfigurationMenu(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.noneSkinConfigurationEnabled.get(),
        COMMON.noneSkinConfigurationAllowInCreative.get(),
        COMMON.noneSkinConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          NoneSkinConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openNoneDialogConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.noneDialogConfigurationEnabled.get(),
        COMMON.noneDialogConfigurationAllowInCreative.get(),
        COMMON.noneDialogConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          NoneDialogConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), dialogDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(dialogDataSet.createTag());
          });
    }
  }

  public static void openPlayerSkinConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.playerSkinConfigurationEnabled.get(),
        COMMON.playerSkinConfigurationAllowInCreative.get(),
        COMMON.playerSkinConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          PlayerSkinConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openUrlSkinConfigurationMenu(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.urlSkinConfigurationEnabled.get(),
        COMMON.urlSkinConfigurationAllowInCreative.get(),
        COMMON.urlSkinConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          UrlSkinConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openScalingConfigurationMenu(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.scalingConfigurationEnabled.get(),
        COMMON.scalingConfigurationAllowInCreative.get(),
        COMMON.scalingConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          ScalingConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openCustomPresetExportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.customExportPresetConfigurationEnabled.get(),
        COMMON.customExportPresetConfigurationAllowInCreative.get(),
        COMMON.customExportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          CustomExportPresetConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openWorldPresetExportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.worldExportPresetConfigurationEnabled.get(),
        COMMON.worldExportPresetConfigurationAllowInCreative.get(),
        COMMON.worldExportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          WorldExportPresetConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openDefaultPresetImportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.defaultImportPresetConfigurationEnabled.get(),
        COMMON.defaultImportPresetConfigurationAllowInCreative.get(),
        COMMON.defaultImportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          DefaultImportPresetConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openServerPresetImportConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.worldImportPresetConfigurationEnabled.get(),
        COMMON.worldImportPresetConfigurationAllowInCreative.get(),
        COMMON.worldImportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      List<ResourceLocation> worldPresets =
          WorldPresetDataFiles.getPresetFilePathResourceLocations().toList();
      NetworkHooks.openScreen(
          serverPlayer,
          WorldImportPresetConfigurationMenu.getMenuProvider(
              uuid, easyNPC.getEntity(), worldPresets),
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
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.customImportPresetConfigurationEnabled.get(),
        COMMON.customImportPresetConfigurationAllowInCreative.get(),
        COMMON.customImportPresetConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          CustomImportPresetConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openNoneTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.noneTradingConfigurationEnabled.get(),
        COMMON.noneTradingConfigurationAllowInCreative.get(),
        COMMON.noneTradingConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          NoneTradingConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBasicTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.basicTradingConfigurationEnabled.get(),
        COMMON.basicTradingConfigurationAllowInCreative.get(),
        COMMON.basicTradingConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          BasicTradingConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openAdvancedTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, int pageIndex) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.advancedTradingConfigurationEnabled.get(),
        COMMON.advancedTradingConfigurationAllowInCreative.get(),
        COMMON.advancedTradingConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          AdvancedTradingConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity(), pageIndex),
          buffer -> buffer.writeUUID(uuid).writeInt(pageIndex));
    }
  }

  public static void openCustomTradingConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.customTradingConfigurationEnabled.get(),
        COMMON.customTradingConfigurationAllowInCreative.get(),
        COMMON.customTradingConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          CustomTradingConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBasicObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.basicObjectiveConfigurationEnabled.get(),
        COMMON.basicObjectiveConfigurationAllowInCreative.get(),
        COMMON.basicObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      ObjectiveDataSet objectiveDataSet = easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          BasicObjectiveConfigurationMenu.getMenuProvider(
              uuid, easyNPC.getEntity(), objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openAttackObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.attackObjectiveConfigurationEnabled.get(),
        COMMON.attackObjectiveConfigurationAllowInCreative.get(),
        COMMON.attackObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      ObjectiveDataSet objectiveDataSet = easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          AttackObjectiveConfigurationMenu.getMenuProvider(
              uuid, easyNPC.getEntity(), objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openFollowObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.followObjectiveConfigurationEnabled.get(),
        COMMON.followObjectiveConfigurationAllowInCreative.get(),
        COMMON.followObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      ObjectiveDataSet objectiveDataSet = easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          FollowObjectiveConfigurationMenu.getMenuProvider(
              uuid, easyNPC.getEntity(), objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openLookObjectiveConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.lookObjectiveConfigurationEnabled.get(),
        COMMON.lookObjectiveConfigurationAllowInCreative.get(),
        COMMON.lookObjectiveConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      ObjectiveDataSet objectiveDataSet = easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet();
      NetworkHooks.openScreen(
          serverPlayer,
          LookObjectiveConfigurationMenu.getMenuProvider(
              uuid, easyNPC.getEntity(), objectiveDataSet),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeNbt(objectiveDataSet.createTag());
          });
    }
  }

  public static void openAbilitiesAttributeConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.abilitiesAttributeConfigurationEnabled.get(),
        COMMON.abilitiesAttributeConfigurationAllowInCreative.get(),
        COMMON.abilitiesAttributeConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          AbilitiesAttributeConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  public static void openBaseAttributeConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.baseAttributeConfigurationEnabled.get(),
        COMMON.baseAttributeConfigurationAllowInCreative.get(),
        COMMON.baseAttributeConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      LivingEntity livingEntity = easyNPC.getLivingEntity();
      NetworkHooks.openScreen(
          serverPlayer,
          BaseAttributeConfigurationMenu.getMenuProvider(uuid, livingEntity),
          buffer -> {
            buffer.writeUUID(uuid);
            buffer.writeDouble(
                livingEntity.getAttribute(Attributes.FOLLOW_RANGE) != null
                    ? livingEntity.getAttribute(Attributes.FOLLOW_RANGE).getBaseValue()
                    : 32.0d);
            buffer.writeDouble(
                livingEntity.getAttribute(Attributes.KNOCKBACK_RESISTANCE) != null
                    ? livingEntity.getAttribute(Attributes.KNOCKBACK_RESISTANCE).getBaseValue()
                    : 0.0d);
            buffer.writeDouble(
                livingEntity.getAttribute(Attributes.ATTACK_DAMAGE) != null
                    ? livingEntity.getAttribute(Attributes.ATTACK_DAMAGE).getBaseValue()
                    : 2.0d);
            buffer.writeDouble(
                livingEntity.getAttribute(Attributes.ATTACK_KNOCKBACK) != null
                    ? livingEntity.getAttribute(Attributes.ATTACK_KNOCKBACK).getBaseValue()
                    : 0.0d);
          });
    }
  }

  public static void openDisplayAttributeConfigurationMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (hasPermissions(
        serverPlayer,
        easyNPC,
        COMMON.displayAttributeConfigurationEnabled.get(),
        COMMON.displayAttributeConfigurationAllowInCreative.get(),
        COMMON.displayAttributeConfigurationPermissionLevel.get())) {
      UUID uuid = easyNPC.getUUID();
      NetworkHooks.openScreen(
          serverPlayer,
          DisplayAttributeConfigurationMenu.getMenuProvider(uuid, easyNPC.getEntity()),
          buffer -> buffer.writeUUID(uuid));
    }
  }

  private static boolean hasPermissions(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      Boolean enabled,
      Boolean allowInCreative,
      int permissionLevel) {
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (Boolean.FALSE.equals(enabled) || serverPlayer == null) {
      return false;
    } else if (Boolean.TRUE.equals(allowInCreative) && serverPlayer.isCreative()) {
      return true;
    } else if (!ownerData.hasOwner() || !ownerData.isOwner(serverPlayer)) {
      return false;
    } else return serverPlayer.hasPermissions(permissionLevel);
  }

  @Override
  public void openDialogTextEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      ConfigurationType formerConfigurationType,
      int pageIndex) {
    UUID uuid = easyNPC.getUUID();
    DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    NetworkHooks.openScreen(
        serverPlayer,
        DialogTextEditorMenu.getMenuProvider(
            uuid, easyNPC.getEntity(), dialogDataSet, dialogId, formerConfigurationType, pageIndex),
        buffer -> {
          buffer.writeUUID(uuid);
          buffer.writeNbt(dialogDataSet.createTag());
          buffer.writeUUID(dialogId);
          buffer.writeEnum(formerConfigurationType);
          buffer.writeInt(pageIndex);
        });
  }

  @Override
  public void openDialogEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      ConfigurationType formerConfigurationType,
      int pageIndex) {
    UUID uuid = easyNPC.getUUID();
    DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    NetworkHooks.openScreen(
        serverPlayer,
        DialogEditorMenu.getMenuProvider(
            uuid, easyNPC.getEntity(), dialogDataSet, dialogId, formerConfigurationType, pageIndex),
        buffer -> {
          buffer.writeUUID(uuid);
          buffer.writeNbt(dialogDataSet.createTag());
          buffer.writeUUID(dialogId);
          buffer.writeEnum(formerConfigurationType);
          buffer.writeInt(pageIndex);
        });
  }

  @Override
  public void openDialogButtonEditorMenu(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      UUID dialogId,
      UUID dialogButtonId,
      ConfigurationType formerConfigurationType,
      int pageIndex) {
    UUID uuid = easyNPC.getUUID();
    DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    NetworkHooks.openScreen(
        serverPlayer,
        DialogButtonEditorMenu.getMenuProvider(
            uuid,
            easyNPC.getEntity(),
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

  @Override
  public void openConfigurationMenu(
      ConfigurationType configurationType,
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      int pageIndex) {
    switch (configurationType) {
      case ATTACK_OBJECTIVE -> openAttackObjectiveConfigurationMenu(serverPlayer, easyNPC);
      case FOLLOW_OBJECTIVE -> openFollowObjectiveConfigurationMenu(serverPlayer, easyNPC);
      case ADVANCED_DIALOG -> openAdvancedDialogConfigurationMenu(serverPlayer, easyNPC);
      case ADVANCED_POSE -> openAdvancedPoseConfigurationMenu(serverPlayer, easyNPC);
      case BASIC_ACTION -> openBasicActionConfigurationMenu(serverPlayer, easyNPC);
      case DIALOG_ACTION -> openDialogActionConfigurationMenu(serverPlayer, easyNPC);
      case DISTANCE_ACTION -> openDistanceActionConfigurationMenu(serverPlayer, easyNPC);
      case BASIC_DIALOG -> openBasicDialogConfigurationMenu(serverPlayer, easyNPC);
      case NONE_SKIN -> openNoneSkinConfigurationMenu(serverPlayer, easyNPC);
      case CUSTOM_SKIN -> openCustomSkinConfigurationMenu(serverPlayer, easyNPC);
      case DEFAULT_SKIN -> openDefaultSkinConfigurationMenu(serverPlayer, easyNPC);
      case EQUIPMENT -> openEquipmentConfigurationMenu(serverPlayer, easyNPC);
      case MAIN -> openMainConfigurationMenu(serverPlayer, easyNPC);
      case DEFAULT_POSE -> openDefaultPoseConfigurationMenu(serverPlayer, easyNPC);
      case CUSTOM_POSE -> openCustomPoseConfigurationMenu(serverPlayer, easyNPC);
      case DEFAULT_POSITION -> openDefaultPositionConfigurationMenu(serverPlayer, easyNPC);
      case DEFAULT_ROTATION -> openDefaultRotationConfigurationMenu(serverPlayer, easyNPC);
      case NONE_DIALOG -> openNoneDialogConfigurationMenu(serverPlayer, easyNPC);
      case PLAYER_SKIN -> openPlayerSkinConfigurationMenu(serverPlayer, easyNPC);
      case URL_SKIN -> openUrlSkinConfigurationMenu(serverPlayer, easyNPC);
      case SCALING -> openScalingConfigurationMenu(serverPlayer, easyNPC);
      case YES_NO_DIALOG -> openYesNoDialogConfigurationMenu(serverPlayer, easyNPC);
      case CUSTOM_PRESET_EXPORT -> openCustomPresetExportConfigurationMenu(serverPlayer, easyNPC);
      case WORLD_PRESET_EXPORT -> openWorldPresetExportConfigurationMenu(serverPlayer, easyNPC);
      case DEFAULT_PRESET_IMPORT -> openDefaultPresetImportConfigurationMenu(serverPlayer, easyNPC);
      case WORLD_PRESET_IMPORT -> openServerPresetImportConfigurationMenu(serverPlayer, easyNPC);
      case CUSTOM_PRESET_IMPORT -> openCustomPresetImportConfigurationMenu(serverPlayer, easyNPC);
      case BASIC_TRADING -> openBasicTradingConfigurationMenu(serverPlayer, easyNPC);
      case NONE_TRADING -> openNoneTradingConfigurationMenu(serverPlayer, easyNPC);
      case ADVANCED_TRADING ->
          openAdvancedTradingConfigurationMenu(serverPlayer, easyNPC, pageIndex);
      case CUSTOM_TRADING -> openCustomTradingConfigurationMenu(serverPlayer, easyNPC);
      case BASIC_OBJECTIVE -> openBasicObjectiveConfigurationMenu(serverPlayer, easyNPC);
      case ABILITIES_ATTRIBUTE -> openAbilitiesAttributeConfigurationMenu(serverPlayer, easyNPC);
      case BASE_ATTRIBUTE -> openBaseAttributeConfigurationMenu(serverPlayer, easyNPC);
      case LOOK_OBJECTIVE -> openLookObjectiveConfigurationMenu(serverPlayer, easyNPC);
      case DISPLAY_ATTRIBUTE -> openDisplayAttributeConfigurationMenu(serverPlayer, easyNPC);
      default ->
          log.debug("Unknown dialog {} for {} from {}", configurationType, easyNPC, serverPlayer);
    }
  }

  @Override
  public void openDialogMenu(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, UUID dialogId, int pageIndex) {
    UUID uuid = easyNPC.getUUID();
    ActionEventSet actionDataSet = easyNPC.getEasyNPCActionEventData().getActionEventSet();
    DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    NetworkHooks.openScreen(
        serverPlayer,
        DialogMenu.getMenuProvider(
            uuid, easyNPC.getEntity(), actionDataSet, dialogDataSet, dialogId, pageIndex),
        buffer -> {
          buffer.writeUUID(uuid);
          buffer.writeNbt(actionDataSet.createTag());
          buffer.writeNbt(dialogDataSet.createTag());
          buffer.writeUUID(dialogId);
          buffer.writeInt(pageIndex);
        });
  }
}
