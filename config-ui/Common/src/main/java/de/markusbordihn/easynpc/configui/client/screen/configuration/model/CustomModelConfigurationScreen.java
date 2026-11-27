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

package de.markusbordihn.easynpc.configui.client.screen.configuration.model;

import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.configui.client.renderer.manager.EntityTypeValidator;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import java.util.List;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Inventory;

public class CustomModelConfigurationScreen<T extends ConfigurationMenu>
    extends ModelSelectionConfigurationScreen<T> {

  private int numberOfSupportedEntityTypes = 0;
  private int numberOfUnknownEntityTypes = 0;

  public CustomModelConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.customModelButton.active = false;
    this.setDescriptionText("custom_model.text");
    EntityTypeValidator.validateUnknownEntityTypes(this.minecraftInstance.level);
  }

  @Override
  protected void updateTick() {
    super.updateTick();

    EntityTypeValidator.validateUnknownEntityTypes(this.minecraftInstance.level);
  }

  @Override
  protected List<ResourceLocation> loadEntries() {
    this.numberOfSupportedEntityTypes = EntityTypeManager.getSupportedEntityTypes().size();
    this.numberOfUnknownEntityTypes = EntityTypeManager.getUnknownEntityTypes().size();
    return EntityTypeManager.getUnknownAndSupportedEntityTypes().stream()
        .map(BuiltInRegistries.ENTITY_TYPE::getKey)
        .toList();
  }

  @Override
  protected boolean hasEntriesChanged() {
    return this.numberOfSupportedEntityTypes != EntityTypeManager.getSupportedEntityTypes().size()
        || this.numberOfUnknownEntityTypes != EntityTypeManager.getUnknownEntityTypes().size();
  }

  @Override
  protected boolean isSelectedEntry(ResourceLocation entryId) {
    RenderDataEntry currentEntry = this.getEasyNPC().getEasyNPCRenderData().getRenderDataEntry();
    return BuiltInRegistries.ENTITY_TYPE.get(entryId).equals(currentEntry.getRenderEntityType());
  }

  @Override
  protected void selectEntry(ResourceLocation entryId) {
    NetworkMessageHandlerManager.getServerHandler()
        .setRenderEntityType(this.getEasyNPCUUID(), BuiltInRegistries.ENTITY_TYPE.get(entryId));
  }

  @Override
  protected void renderEntryPreview(
      GuiGraphics guiGraphics, int x, int y, ResourceLocation entryId) {
    EntityType<?> entityType = BuiltInRegistries.ENTITY_TYPE.get(entryId);
    int previewScale = Math.round(30 / EntityTypeManager.getScaleFactor(entityType));
    int previewCenterPos =
        y - 2 - Math.round(this.getEasyNPC().getLivingEntity().getBbHeight() / 2f * previewScale);
    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.customModel(x + 5, previewCenterPos, previewScale, entityType),
        this.xMouse,
        this.yMouse);
  }
}
