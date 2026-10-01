/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.clienttest;

import java.io.IOException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class ConfigurationSubPageClientTest extends ClientTestBase {

  @ParameterizedTest(name = "{0} > {1}")
  @CsvSource({
    "actions, basic, BasicActionConfigurationScreen",
    "actions, dialog_actions, DialogActionConfigurationScreen",
    "actions, distance_actions, DistanceActionConfigurationScreen",
    "actions, interval_actions, IntervalActionConfigurationScreen",
    "attributes, abilities, AbilitiesAttributeConfigurationScreen",
    "attributes, base, BaseAttributeConfigurationScreen",
    "attributes, combat, CombatAttributeConfigurationScreen",
    "attributes, display, DisplayAttributeConfigurationScreen",
    "attributes, misc, MiscAttributeConfigurationScreen",
    "dialog, disable_dialog, NoneDialogConfigurationScreen",
    "dialog, basic, BasicDialogConfigurationScreen",
    "dialog, yes_no_dialog, YesNoDialogConfigurationScreen",
    "dialog, advanced, AdvancedDialogConfigurationScreen",
    "objective, basic, BasicObjectiveConfigurationScreen",
    "objective, follow, FollowObjectiveConfigurationScreen",
    "objective, attack, AttackObjectiveConfigurationScreen",
    "objective, target, TargetObjectiveConfigurationScreen",
    "objective, flee, FleeObjectiveConfigurationScreen",
    "objective, look, LookObjectiveConfigurationScreen",
    "pose, default, DefaultPoseConfigurationScreen",
    "pose, basic, BasicPoseConfigurationScreen",
    "pose, advanced, AdvancedPoseConfigurationScreen",
    "pose, custom, CustomPoseConfigurationScreen",
    "edit_skin, default, DefaultSkinConfigurationScreen",
    "edit_skin, player_skin, PlayerSkinConfigurationScreen",
    "edit_skin, url_skin, UrlSkinConfigurationScreen",
    "edit_skin, custom, CustomSkinConfigurationScreen",
    "edit_skin, advanced_skin, AdvancedSkinConfigurationScreen",
    "change_model, default, DefaultModelConfigurationScreen",
    "change_model, custom, CustomModelConfigurationScreen",
    "sound, basic, BasicSoundConfigurationScreen",
    "sound, combat, CombatSoundConfigurationScreen",
    "sound, interaction, InteractionSoundConfigurationScreen",
    "sound, trade, TradeSoundConfigurationScreen",
    "trading, disable_trading, NoneTradingConfigurationContainerScreen",
    "trading, basic, BasicTradingConfigurationContainerScreen",
    "trading, advanced, AdvancedTradingConfigurationContainerScreen",
    "trading, custom, CustomTradingConfigurationContainerScreen",
    "export, local, ExportLocalPresetConfigurationScreen",
    "export, custom, ExportCustomPresetConfigurationScreen",
    "export, world_preset, ExportWorldPresetConfigurationScreen",
    "import, local, ImportLocalPresetConfigurationScreen",
    "import, default, ImportDefaultPresetConfigurationScreen",
    "import, world_preset, ImportWorldPresetConfigurationScreen",
    "import, custom, ImportCustomPresetConfigurationScreen"
  })
  @DisplayName("Configuration tab opens and renders its sub page")
  void tabOpensSubPage(String categoryLabel, String tabLabel, String screenClassName)
      throws IOException {
    summonTestNpc();
    openMainConfiguration();

    openSubPage(categoryLabel, tabLabel, screenClassName);

    captureScreen(categoryLabel + "-" + tabLabel);
  }
}
