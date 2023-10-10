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

package de.markusbordihn.easynpc.config;

import java.nio.file.Files;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.loading.FMLPaths;

import de.markusbordihn.easynpc.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class CommonConfig {
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private CommonConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} common config ...", Constants.MOD_NAME);
    try {
      Files.createDirectories(FMLPaths.CONFIGDIR.get().resolve(Constants.MOD_ID));
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        Constants.MOD_ID + "/" + Constants.MOD_ID + "-common.toml");
  }

  public static class Config {

    public static final String MAIN_CONFIGURATION = "main";
    public final ForgeConfigSpec.BooleanValue mainConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue mainConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue mainConfigurationPermissionLevel;

    public static final String BASIC_ACTION_CONFIGURATION = "basic action";
    public final ForgeConfigSpec.BooleanValue basicActionConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue basicActionConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue basicActionConfigurationPermissionLevel;

    public static final String DIALOG_ACTION_CONFIGURATION = "dialog action";
    public final ForgeConfigSpec.BooleanValue dialogActionConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue dialogActionConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue dialogActionConfigurationPermissionLevel;

    public static final String DISTANCE_ACTION_CONFIGURATION = "distance action";
    public final ForgeConfigSpec.BooleanValue distanceActionConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue distanceActionConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue distanceActionConfigurationPermissionLevel;

    public static final String BASIC_DIALOG_CONFIGURATION = "basic dialog";
    public final ForgeConfigSpec.BooleanValue basicDialogConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue basicDialogConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue basicDialogConfigurationPermissionLevel;

    public static final String YES_NO_DIALOG_CONFIGURATION = "yes/no dialog";
    public final ForgeConfigSpec.BooleanValue yesNoDialogConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue yesNoDialogConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue yesNoDialogConfigurationPermissionLevel;

    public static final String NONE_DIALOG_CONFIGURATION = "none dialog";
    public final ForgeConfigSpec.BooleanValue noneDialogConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue noneDialogConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue noneDialogConfigurationPermissionLevel;

    public static final String DEFAULT_SKIN_CONFIGURATION = "default skin";
    public final ForgeConfigSpec.BooleanValue defaultSkinConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultSkinConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultSkinConfigurationPermissionLevel;

    public static final String PLAYER_SKIN_CONFIGURATION = "player skin";
    public final ForgeConfigSpec.BooleanValue playerSkinConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue playerSkinConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue playerSkinConfigurationPermissionLevel;

    public static final String CUSTOM_SKIN_CONFIGURATION = "custom skin";
    public final ForgeConfigSpec.BooleanValue customSkinConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue customSkinConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue customSkinConfigurationPermissionLevel;

    public static final String DEFAULT_POSE_CONFIGURATION = "default pose";
    public final ForgeConfigSpec.BooleanValue defaultPoseConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultPoseConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultPoseConfigurationPermissionLevel;

    private static final String ADVANCED_POSE_CONFIGURATION = "advanced pose";
    public final ForgeConfigSpec.BooleanValue advancedPoseConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue advancedPoseConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue advancedPoseConfigurationPermissionLevel;

    public static final String CUSTOM_POSE_CONFIGURATION = "custom pose";
    public final ForgeConfigSpec.BooleanValue customPoseConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue customPoseConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue customPoseConfigurationPermissionLevel;

    public static final String DEFAULT_EQUIPMENT_CONFIGURATION = "equipment";
    public final ForgeConfigSpec.BooleanValue equipmentConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue equipmentConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue equipmentConfigurationPermissionLevel;

    public static final String DEFAULT_POSITION_CONFIGURATION = "position";
    public final ForgeConfigSpec.BooleanValue defaultPositionConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultPositionConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultPositionConfigurationPermissionLevel;

    public static final String DEFAULT_ROTATION_CONFIGURATION = "rotation";
    public final ForgeConfigSpec.BooleanValue defaultRotationConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultRotationConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultRotationConfigurationPermissionLevel;

    public static final String DEFAULT_SCALE_CONFIGURATION = "scale";
    public final ForgeConfigSpec.BooleanValue scalingConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue scalingConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue scalingConfigurationPermissionLevel;

    public static final String CUSTOM_EXPORT_PRESET_CONFIGURATION = "custom export preset";
    public final ForgeConfigSpec.BooleanValue customExportPresetConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue customExportPresetConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue customExportPresetConfigurationPermissionLevel;

    public static final String WORLD_EXPORT_PRESET_CONFIGURATION = "world export preset";
    public final ForgeConfigSpec.BooleanValue worldExportPresetConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue worldExportPresetConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue worldExportPresetConfigurationPermissionLevel;

    public static final String CUSTOM_IMPORT_PRESET_CONFIGURATION = "custom import preset";
    public final ForgeConfigSpec.BooleanValue customImportPresetConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue customImportPresetConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue customImportPresetConfigurationPermissionLevel;

    public static final String DEFAULT_IMPORT_PRESET_CONFIGURATION = "default import preset";
    public final ForgeConfigSpec.BooleanValue defaultImportPresetConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultImportPresetConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultImportPresetConfigurationPermissionLevel;

    public static final String WORLD_IMPORT_PRESET_CONFIGURATION = "world import preset";
    public final ForgeConfigSpec.BooleanValue worldImportPresetConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue worldImportPresetConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue worldImportPresetConfigurationPermissionLevel;

    public static final String NONE_TRADING_CONFIGURATION = "none trading";
    public final ForgeConfigSpec.BooleanValue noneTradingConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue noneTradingConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue noneTradingConfigurationPermissionLevel;

    public static final String BASIC_TRADING_CONFIGURATION = "basic trading";
    public final ForgeConfigSpec.BooleanValue basicTradingConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue basicTradingConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue basicTradingConfigurationPermissionLevel;

    public static final String ADVANCED_TRADING_CONFIGURATION = "advanced trading";
    public final ForgeConfigSpec.BooleanValue advancedTradingConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue advancedTradingConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue advancedTradingConfigurationPermissionLevel;

    public static final String CUSTOM_TRADING_CONFIGURATION = "custom trading";
    public final ForgeConfigSpec.BooleanValue customTradingConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue customTradingConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue customTradingConfigurationPermissionLevel;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("General");
      builder.pop();

      builder.push("[Main Configuration]");
      mainConfigurationEnabled = builder.comment(getEnableComment(MAIN_CONFIGURATION))
          .define("mainConfigurationEnabled", true);
      mainConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(MAIN_CONFIGURATION))
              .define("mainConfigurationAllowInCreative", true);
      mainConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(MAIN_CONFIGURATION))
              .defineInRange("mainConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Action Configuration] Basic Action");
      basicActionConfigurationEnabled =
          builder.comment(getEnableComment(BASIC_ACTION_CONFIGURATION))
              .define("basicActionConfigurationEnabled", true);
      basicActionConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(BASIC_ACTION_CONFIGURATION))
              .define("basicActionConfigurationAllowInCreative", true);
      basicActionConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(BASIC_ACTION_CONFIGURATION))
              .defineInRange("basicActionConfigurationPermissionLevel", 1, 0, 4);
      builder.pop();

      builder.push("[Action Configuration] Dialog Action");
      dialogActionConfigurationEnabled =
          builder.comment(getEnableComment(DIALOG_ACTION_CONFIGURATION))
              .define("dialogActionConfigurationEnabled", true);
      dialogActionConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DIALOG_ACTION_CONFIGURATION))
              .define("dialogActionConfigurationAllowInCreative", true);
      dialogActionConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DIALOG_ACTION_CONFIGURATION))
              .defineInRange("dialogActionConfigurationPermissionLevel", 1, 0, 4);
      builder.pop();

      builder.push("[Action Configuration] Distance Action");
      distanceActionConfigurationEnabled =
          builder.comment(getEnableComment(DISTANCE_ACTION_CONFIGURATION))
              .define("distanceActionConfigurationEnabled", true);
      distanceActionConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DISTANCE_ACTION_CONFIGURATION))
              .define("distanceActionConfigurationAllowInCreative", true);
      distanceActionConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DISTANCE_ACTION_CONFIGURATION))
              .defineInRange("distanceActionConfigurationPermissionLevel", 1, 0, 4);
      builder.pop();

      builder.push("[Dialog Configuration] None Dialog");
      noneDialogConfigurationEnabled = builder.comment(getEnableComment(NONE_DIALOG_CONFIGURATION))
          .define("noneDialogConfigurationEnabled", true);
      noneDialogConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(NONE_DIALOG_CONFIGURATION))
              .define("noneDialogConfigurationAllowInCreative", true);
      noneDialogConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(NONE_DIALOG_CONFIGURATION))
              .defineInRange("noneDialogConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Dialog Configuration] Basic Dialog");
      basicDialogConfigurationEnabled =
          builder.comment(getEnableComment(BASIC_DIALOG_CONFIGURATION))
              .define("basicDialogConfigurationEnabled", true);
      basicDialogConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(BASIC_DIALOG_CONFIGURATION))
              .define("basicDialogConfigurationAllowInCreative", true);
      basicDialogConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(BASIC_DIALOG_CONFIGURATION))
              .defineInRange("basicDialogConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Dialog Configuration] Yes/No Dialog");
      yesNoDialogConfigurationEnabled =
          builder.comment(getEnableComment(YES_NO_DIALOG_CONFIGURATION))
              .define("yesNoDialogConfigurationEnabled", true);
      yesNoDialogConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(YES_NO_DIALOG_CONFIGURATION))
              .define("yesNoDialogConfigurationAllowInCreative", true);
      yesNoDialogConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(YES_NO_DIALOG_CONFIGURATION))
              .defineInRange("yesNoDialogConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Skin Configuration] Default Skin");
      defaultSkinConfigurationEnabled =
          builder.comment(getEnableComment(DEFAULT_SKIN_CONFIGURATION))
              .define("defaultSkinConfigurationEnabled", true);
      defaultSkinConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DEFAULT_SKIN_CONFIGURATION))
              .define("defaultSkinConfigurationAllowInCreative", true);
      defaultSkinConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DEFAULT_SKIN_CONFIGURATION))
              .defineInRange("defaultSkinConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Skin Configuration] Player Skin");
      playerSkinConfigurationEnabled = builder.comment(getEnableComment(PLAYER_SKIN_CONFIGURATION))
          .define("playerSkinConfigurationEnabled", true);
      playerSkinConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(PLAYER_SKIN_CONFIGURATION))
              .define("playerSkinConfigurationAllowInCreative", true);
      playerSkinConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(PLAYER_SKIN_CONFIGURATION))
              .defineInRange("playerSkinConfigurationPermissionLevel", 1, 0, 4);
      builder.pop();

      builder.push("[Skin Configuration] Custom Skin");
      customSkinConfigurationEnabled = builder.comment(getEnableComment(CUSTOM_SKIN_CONFIGURATION))
          .define("customSkinConfigurationEnabled", true);
      customSkinConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(CUSTOM_SKIN_CONFIGURATION))
              .define("customSkinConfigurationAllowInCreative", true);
      customSkinConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(CUSTOM_SKIN_CONFIGURATION))
              .defineInRange("customSkinConfigurationPermissionLevel", 2, 0, 4);
      builder.pop();

      builder.push("[Pose Configuration] Default Pose");
      defaultPoseConfigurationEnabled =
          builder.comment(getEnableComment(DEFAULT_POSE_CONFIGURATION))
              .define("defaultPoseConfigurationEnabled", true);
      defaultPoseConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DEFAULT_POSE_CONFIGURATION))
              .define("defaultPoseConfigurationAllowInCreative", true);
      defaultPoseConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DEFAULT_POSE_CONFIGURATION))
              .defineInRange("defaultPoseConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Pose Configuration] Advanced Pose");
      advancedPoseConfigurationEnabled =
          builder.comment(getEnableComment(ADVANCED_POSE_CONFIGURATION))
              .define("advancedPoseConfigurationEnabled", true);
      advancedPoseConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(ADVANCED_POSE_CONFIGURATION))
              .define("advancedPoseConfigurationAllowInCreative", true);
      advancedPoseConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(ADVANCED_POSE_CONFIGURATION))
              .defineInRange("advancedPoseConfigurationPermissionLevel", 0, 0, 4);

      builder.push("[Pose Configuration] Custom Pose");
      customPoseConfigurationEnabled = builder.comment(getEnableComment(CUSTOM_POSE_CONFIGURATION))
          .define("customPoseConfigurationEnabled", true);
      customPoseConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(CUSTOM_POSE_CONFIGURATION))
              .define("customPoseConfigurationAllowInCreative", true);
      customPoseConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(CUSTOM_POSE_CONFIGURATION))
              .defineInRange("customPoseConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Equipment Configuration]");
      equipmentConfigurationEnabled =
          builder.comment(getEnableComment(DEFAULT_EQUIPMENT_CONFIGURATION))
              .define("equipmentConfigurationEnabled", true);
      equipmentConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DEFAULT_EQUIPMENT_CONFIGURATION))
              .define("equipmentConfigurationAllowInCreative", true);
      equipmentConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DEFAULT_EQUIPMENT_CONFIGURATION))
              .defineInRange("equipmentConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Position Configuration]");
      defaultPositionConfigurationEnabled =
          builder.comment(getEnableComment(DEFAULT_POSITION_CONFIGURATION))
              .define("defaultPositionConfigurationEnabled", true);
      defaultPositionConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DEFAULT_POSITION_CONFIGURATION))
              .define("defaultPositionConfigurationAllowInCreative", true);
      defaultPositionConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DEFAULT_POSITION_CONFIGURATION))
              .defineInRange("defaultPositionConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Rotation Configuration]");
      defaultRotationConfigurationEnabled =
          builder.comment(getEnableComment(DEFAULT_ROTATION_CONFIGURATION))
              .define("defaultRotationConfigurationEnabled", true);
      defaultRotationConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DEFAULT_ROTATION_CONFIGURATION))
              .define("defaultRotationConfigurationAllowInCreative", true);
      defaultRotationConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DEFAULT_ROTATION_CONFIGURATION))
              .defineInRange("defaultRotationConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Scale Configuration]");
      scalingConfigurationEnabled = builder.comment(getEnableComment(DEFAULT_SCALE_CONFIGURATION))
          .define("scaleConfigurationEnabled", true);
      scalingConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DEFAULT_SCALE_CONFIGURATION))
              .define("scaleConfigurationAllowInCreative", true);
      scalingConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DEFAULT_SCALE_CONFIGURATION))
              .defineInRange("scaleConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Export Configuration] Export Custom Preset");
      customExportPresetConfigurationEnabled =
          builder.comment(getEnableComment(CUSTOM_EXPORT_PRESET_CONFIGURATION))
              .define("customExportPresetConfigurationEnabled", true);
      customExportPresetConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(CUSTOM_EXPORT_PRESET_CONFIGURATION))
              .define("customExportPresetConfigurationAllowInCreative", true);
      customExportPresetConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(CUSTOM_EXPORT_PRESET_CONFIGURATION))
              .defineInRange("customExportPresetConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Export Configuration] Export World Preset");
      worldExportPresetConfigurationEnabled =
          builder.comment(getEnableComment(WORLD_EXPORT_PRESET_CONFIGURATION))
              .define("worldExportPresetConfigurationEnabled", true);
      worldExportPresetConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(WORLD_EXPORT_PRESET_CONFIGURATION))
              .define("worldExportPresetConfigurationAllowInCreative", true);
      worldExportPresetConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(WORLD_EXPORT_PRESET_CONFIGURATION))
              .defineInRange("worldExportPresetConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Import Configuration] Import Default Preset");
      defaultImportPresetConfigurationEnabled =
          builder.comment(getEnableComment(DEFAULT_IMPORT_PRESET_CONFIGURATION))
              .define("defaultImportPresetConfigurationEnabled", true);
      defaultImportPresetConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(DEFAULT_IMPORT_PRESET_CONFIGURATION))
              .define("defaultImportPresetConfigurationAllowInCreative", true);
      defaultImportPresetConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(DEFAULT_IMPORT_PRESET_CONFIGURATION))
              .defineInRange("defaultImportPresetConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Import Configuration] Import Custom Preset");
      customImportPresetConfigurationEnabled =
          builder.comment(getEnableComment(CUSTOM_IMPORT_PRESET_CONFIGURATION))
              .define("customImportPresetConfigurationEnabled", true);
      customImportPresetConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(CUSTOM_IMPORT_PRESET_CONFIGURATION))
              .define("customImportPresetConfigurationAllowInCreative", true);
      customImportPresetConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(CUSTOM_IMPORT_PRESET_CONFIGURATION))
              .defineInRange("customImportPresetConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Import Configuration] Import Server Preset");
      worldImportPresetConfigurationEnabled =
          builder.comment(getEnableComment(WORLD_IMPORT_PRESET_CONFIGURATION))
              .define("worldImportPresetConfigurationEnabled", true);
      worldImportPresetConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(WORLD_IMPORT_PRESET_CONFIGURATION))
              .define("worldImportPresetConfigurationAllowInCreative", true);
      worldImportPresetConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(WORLD_IMPORT_PRESET_CONFIGURATION))
              .defineInRange("worldImportPresetConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Trading Configuration] Default Trading");
      noneTradingConfigurationEnabled =
          builder.comment(getEnableComment(NONE_TRADING_CONFIGURATION))
              .define("noneTradingConfigurationEnabled", true);
      noneTradingConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(NONE_TRADING_CONFIGURATION))
              .define("noneTradingConfigurationAllowInCreative", true);
      noneTradingConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(NONE_TRADING_CONFIGURATION))
              .defineInRange("noneTradingConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Trading Configuration] Basic Trading");
      basicTradingConfigurationEnabled =
          builder.comment(getEnableComment(BASIC_TRADING_CONFIGURATION))
              .define("basicTradingConfigurationEnabled", true);
      basicTradingConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(BASIC_TRADING_CONFIGURATION))
              .define("basicTradingConfigurationAllowInCreative", true);
      basicTradingConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(BASIC_TRADING_CONFIGURATION))
              .defineInRange("basicTradingConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Trading Configuration] Advanced Trading");
      advancedTradingConfigurationEnabled =
          builder.comment(getEnableComment(ADVANCED_TRADING_CONFIGURATION))
              .define("advancedTradingConfigurationEnabled", true);
      advancedTradingConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(ADVANCED_TRADING_CONFIGURATION))
              .define("advancedTradingConfigurationAllowInCreative", true);
      advancedTradingConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(ADVANCED_TRADING_CONFIGURATION))
              .defineInRange("advancedTradingConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("[Trading Configuration] Custom Trading");
      customTradingConfigurationEnabled =
          builder.comment(getEnableComment(CUSTOM_TRADING_CONFIGURATION))
              .define("customTradingConfigurationEnabled", true);
      customTradingConfigurationAllowInCreative =
          builder.comment(getAllowInCreativeComment(CUSTOM_TRADING_CONFIGURATION))
              .define("customTradingConfigurationAllowInCreative", true);
      customTradingConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment(CUSTOM_TRADING_CONFIGURATION))
              .defineInRange("customTradingConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();
    }
  }

  public static String getEnableComment(String name) {
    return "Enable " + name + " configuration.";
  }

  public static String getAllowInCreativeComment(String name) {
    return "Allow " + name + " configuration in creative mode.";
  }

  public static String getPermissionLevelComment(String name) {
    return name
        + " configuration permission level. (0 = everyone, 1 = moderator, 2 = gamemaster, 3 = admin, 4 = owner)";
  }
}
