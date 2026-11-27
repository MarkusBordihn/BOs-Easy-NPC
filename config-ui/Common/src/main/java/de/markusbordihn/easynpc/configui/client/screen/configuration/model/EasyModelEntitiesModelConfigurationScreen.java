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

package de.markusbordihn.easynpc.configui.client.screen.configuration.model;

import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class EasyModelEntitiesModelConfigurationScreen<T extends ConfigurationMenu>
    extends ModelSelectionConfigurationScreen<T> {

  public static final String ADDITIONAL_DATA_PROFILES_TAG = "EasyModelProfiles";

  public EasyModelEntitiesModelConfigurationScreen(
      T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.defaultModelButton.active = false;
    this.customModelButton.active = false;
    this.setDescriptionText("easy_model_entities_model.text");
    this.setNoteText("easy_model_entities_model.note");
  }

  @Override
  protected List<ResourceLocation> loadEntries() {
    if (this.getAdditionalScreenData() != null) {
      ListTag profilesTag = this.getAdditionalScreenData().getList(ADDITIONAL_DATA_PROFILES_TAG);
      if (profilesTag != null && !profilesTag.isEmpty()) {
        return CompoundTagUtils.readResourceLocations(profilesTag).stream()
            .sorted(Comparator.comparing(ResourceLocation::toString))
            .toList();
      }
    }
    return IntegrationRegistry.getModels(EasyModelEntitiesManager.INTEGRATION_ID).stream()
        .map(ResourceLocation::tryParse)
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(ResourceLocation::toString))
        .toList();
  }
}
