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

package de.markusbordihn.easynpc.configui.client.screen;

import de.markusbordihn.easynpc.configui.client.screen.configuration.actions.BasicActionConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.actions.DialogActionConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.actions.DistanceActionConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.actions.IntervalActionConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.attribute.AbilitiesAttributeConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.attribute.BaseAttributeConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.attribute.BossBarAttributeConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.attribute.CombatAttributeConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.attribute.DisplayAttributeConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.attribute.MiscAttributeConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.attribute.ProjectileAttributeConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.dialog.AdvancedDialogConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.dialog.BasicDialogConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.dialog.NoneDialogConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.dialog.YesNoDialogConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.equipment.EquipmentConfigurationContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.inventory.InventoryConfigurationContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.main.MainConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.model.CobblemonModelConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.model.CustomModelConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.model.DefaultModelConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.model.EasyModelEntitiesModelConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.objective.AttackObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.objective.BasicObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.objective.FleeObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.objective.FollowObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.objective.LookObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.objective.TargetObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.pose.AdvancedPoseConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.pose.BasicPoseConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.pose.CustomPoseConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.pose.DefaultPoseConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.position.DefaultPositionConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.preset.ExportCustomPresetConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.preset.ExportLocalPresetConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.preset.ExportWorldPresetConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.preset.ImportCustomPresetConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.preset.ImportDefaultPresetConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.preset.ImportLocalPresetConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.preset.ImportWorldPresetConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.rotation.DefaultRotationConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.scaling.ScalingConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.skin.AdvancedSkinConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.skin.CustomSkinConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.skin.DefaultSkinConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.skin.PlayerSkinConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.skin.UrlSkinConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.sound.BasicSoundConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.sound.CombatSoundConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.sound.InteractionSoundConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.sound.TradeSoundConfigurationScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.trading.AdvancedTradingConfigurationContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.trading.BasicTradingConfigurationContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.trading.CustomTradingConfigurationContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.configuration.trading.NoneTradingConfigurationContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEditorContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEditorContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.dialog.DialogButtonEditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.dialog.DialogEditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.dialog.DialogOptionsEditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.dialog.DialogTextEditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.faction.FactionEditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.editor.faction.FactionsEditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.preset.PresetBrowserScreen;
import de.markusbordihn.easynpc.configui.menu.ModMenuTypes;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.equipment.EquipmentConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.inventory.InventoryConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.trading.AdvancedTradingConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.trading.BasicTradingConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import net.minecraft.client.gui.screens.MenuScreens;

public class ClientScreens {

  private ClientScreens() {}

  public static void registerScreens() {
    MenuScreens.register(
        ModMenuTypes.ABILITIES_ATTRIBUTE_CONFIGURATION_MENU,
        AbilitiesAttributeConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.ACTION_DATA_EDITOR_MENU, ActionDataEditorContainerScreen<EditorMenu>::new);
    MenuScreens.register(
        ModMenuTypes.ACTION_DATA_ENTRY_EDITOR_MENU,
        ActionDataEntryEditorContainerScreen<EditorMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CONDITION_DATA_EDITOR_MENU,
        ConditionDataEditorContainerScreen<EditorMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CONDITION_DATA_ENTRY_EDITOR_MENU,
        ConditionDataEntryEditorContainerScreen<EditorMenu>::new);
    MenuScreens.register(
        ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU,
        AdvancedDialogConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU,
        AdvancedPoseConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU,
        AdvancedTradingConfigurationContainerScreen<AdvancedTradingConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU,
        AttackObjectiveConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.TARGET_OBJECTIVE_CONFIGURATION_MENU,
        TargetObjectiveConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.FLEE_OBJECTIVE_CONFIGURATION_MENU,
        FleeObjectiveConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU,
        BaseAttributeConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU,
        BasicActionConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU,
        BasicDialogConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU,
        BasicObjectiveConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_POSE_CONFIGURATION_MENU,
        BasicPoseConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU,
        BasicTradingConfigurationContainerScreen<BasicTradingConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_SOUND_CONFIGURATION_MENU,
        BasicSoundConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.COMBAT_ATTRIBUTE_CONFIGURATION_MENU,
        CombatAttributeConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.PROJECTILE_ATTRIBUTE_CONFIGURATION_MENU,
        ProjectileAttributeConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.BOSS_BAR_ATTRIBUTE_CONFIGURATION_MENU,
        BossBarAttributeConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.COMBAT_SOUND_CONFIGURATION_MENU,
        CombatSoundConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU,
        ExportCustomPresetConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.LOCAL_EXPORT_PRESET_CONFIGURATION_MENU,
        ExportLocalPresetConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportCustomPresetConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_POSE_CONFIGURATION_MENU,
        CustomPoseConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU,
        CustomSkinConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU,
        CustomTradingConfigurationContainerScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.COBBLEMON_MODEL_CONFIGURATION_MENU,
        CobblemonModelConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_MODEL_CONFIGURATION_MENU,
        CustomModelConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.EASY_MODEL_ENTITIES_MODEL_CONFIGURATION_MENU,
        EasyModelEntitiesModelConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportDefaultPresetConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_MODEL_CONFIGURATION_MENU,
        DefaultModelConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU,
        DefaultPoseConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_POSITION_CONFIGURATION_MENU,
        DefaultPositionConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_ROTATION_CONFIGURATION_MENU,
        DefaultRotationConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU,
        DefaultSkinConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU,
        DialogActionConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DIALOG_BUTTON_EDITOR_MENU, DialogButtonEditorScreen<EditorMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DIALOG_OPTIONS_EDITOR_MENU, DialogOptionsEditorScreen<EditorMenu>::new);
    MenuScreens.register(ModMenuTypes.DIALOG_EDITOR_MENU, DialogEditorScreen<EditorMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DIALOG_TEXT_EDITOR_MENU, DialogTextEditorScreen<EditorMenu>::new);
    MenuScreens.register(ModMenuTypes.FACTION_EDITOR_MENU, FactionEditorScreen<EditorMenu>::new);
    MenuScreens.register(ModMenuTypes.FACTIONS_EDITOR_MENU, FactionsEditorScreen<EditorMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU,
        DisplayAttributeConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU,
        DistanceActionConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.INTERACTION_SOUND_CONFIGURATION_MENU,
        InteractionSoundConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.TRADE_SOUND_CONFIGURATION_MENU,
        TradeSoundConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.INTERVAL_ACTION_CONFIGURATION_MENU,
        IntervalActionConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU,
        EquipmentConfigurationContainerScreen<EquipmentConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.INVENTORY_CONFIGURATION_MENU,
        InventoryConfigurationContainerScreen<InventoryConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU,
        FollowObjectiveConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.LOCAL_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportLocalPresetConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU,
        LookObjectiveConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.MAIN_CONFIGURATION_MENU, MainConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.MISC_ATTRIBUTE_CONFIGURATION_MENU,
        MiscAttributeConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.NONE_DIALOG_CONFIGURATION_MENU,
        NoneDialogConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.ADVANCED_SKIN_CONFIGURATION_MENU,
        AdvancedSkinConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.NONE_TRADING_CONFIGURATION_MENU,
        NoneTradingConfigurationContainerScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU,
        PlayerSkinConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(ModMenuTypes.PRESET_BROWSER_MENU, PresetBrowserScreen::new);
    MenuScreens.register(
        ModMenuTypes.SCALING_CONFIGURATION_MENU,
        ScalingConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.URL_SKIN_CONFIGURATION_MENU,
        UrlSkinConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.WORLD_EXPORT_PRESET_CONFIGURATION_MENU,
        ExportWorldPresetConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportWorldPresetConfigurationScreen<ConfigurationMenu>::new);
    MenuScreens.register(
        ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU,
        YesNoDialogConfigurationScreen<ConfigurationMenu>::new);
  }
}
