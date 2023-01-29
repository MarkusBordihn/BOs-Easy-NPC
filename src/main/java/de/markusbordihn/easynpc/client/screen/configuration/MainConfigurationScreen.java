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

package de.markusbordihn.easynpc.client.screen.configuration;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.MainConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;

@OnlyIn(Dist.CLIENT)
public class MainConfigurationScreen extends AbstractContainerScreen<MainConfigurationMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final EasyNPCEntity entity;
  protected final UUID uuid;

  // Internal
  protected Button editDialogButton;
  protected Button saveNameButton = null;
  private Button skinPreviousButton = null;
  private Button skinNextButton = null;
  private Button skinPreviousPageButton = null;
  private Button skinNextPageButton = null;
  private List<Button> skinButtons = new ArrayList<>();
  private EditBox nameBox;
  private float xMouse;
  private float yMouse;

  // Skin Preview
  private int skinStartIndex = 0;
  private int maxSkinsPerPage = 5;

  // Cache
  private Enum<?>[] professions;
  private Enum<?>[] variants;
  protected int numOfProfessions = 0;
  protected int numOfSkins = 0;
  protected int numOfVariants = 0;

  public MainConfigurationScreen(MainConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
    this.entity = menu.getEntity();
    this.uuid = this.entity.getUUID();
  }

  private void saveName() {
    String value = this.nameBox.getValue();
    if (value != null && !value.isBlank()) {
      log.debug("Saving name {} for {}", value, this.entity);
      NetworkHandler.nameChange(this.uuid, value);
    }
  }

  private void renderSkins(PoseStack poseStack, float partialTicks) {
    if (this.entity == null) {
      return;
    }

    int positionTop = 100;
    int skinPosition = 0;
    skinButtons = new ArrayList<>();
    for (int i = skinStartIndex; i < this.numOfSkins && i < skinStartIndex + maxSkinsPerPage; i++) {
      int variantIndex = this.numOfProfessions > 0 ? i / this.numOfProfessions : i;
      Enum<?> variant = this.variants[variantIndex];
      int left = this.leftPos + 40 + (skinPosition * 48);
      int top = this.topPos + 60 + positionTop;

      if (Constants.IS_MOD_DEV) {
        // Render skin position
        this.font.draw(poseStack, new TextComponent(i + ""), left - 6f, top - 70f, 4210752);
      }

      // Render additional Professions, if any.
      if (this.numOfProfessions > 0) {
        Enum<?> profession = this.professions[i - (variantIndex * this.numOfProfessions)];
        this.renderSkinEntity(poseStack, left, top, partialTicks, variant, profession);
      } else {
        this.renderSkinEntity(poseStack, left, top, partialTicks, variant, null);
      }
      skinPosition++;
    }
  }

  private void renderSkinEntity(PoseStack poseStack, int x, int y, float partialTicks,
      Enum<?> variant, Enum<?> profession) {
    // Get relevant texture for the preview
    ResourceLocation variantResourceLocation = this.entity.getTextureLocation(variant);
    ResourceLocation professionResourceLocation =
        profession != null ? this.entity.getProfessionTextureLocation(profession) : null;

    // Render skin entity with variant and profession.
    ScreenHelper.renderEntitySkin(x, y, x - this.xMouse, y - 40 - this.yMouse, this.entity,
        variantResourceLocation, professionResourceLocation);

    // Create dynamically button for each skin variant and profession.
    Button skinButton =
        new Button(x - 20, y + 5, 40, 20, new TranslatableComponent("Select"), button -> {
          NetworkHandler.variantChange(this.uuid, variant);
          if (profession != null) {
            NetworkHandler.professionChange(this.uuid, profession);
          }
        });
    if (this.entity.getVariant().equals(variant)
        && this.entity.getProfession().equals(profession)) {
      skinButton.active = false;
    }
    skinButton.render(poseStack, x, y, partialTicks);
    skinButtons.add(skinButton);
  }

  private void checkSkinButtonState() {
    // Check the visible for the buttons.
    boolean skinButtonShouldBeVisible = this.numOfSkins > this.maxSkinsPerPage;
    this.skinPreviousButton.visible = skinButtonShouldBeVisible;
    this.skinNextButton.visible = skinButtonShouldBeVisible;
    this.skinPreviousPageButton.visible = skinButtonShouldBeVisible;
    this.skinNextPageButton.visible = skinButtonShouldBeVisible;

    // Enable / disable buttons depending on the current skin index.
    this.skinPreviousButton.active = this.skinStartIndex > 0;
    this.skinNextButton.active = this.skinStartIndex + this.maxSkinsPerPage < this.numOfSkins;
    this.skinPreviousPageButton.active = this.skinStartIndex - this.maxSkinsPerPage > 0;
    this.skinNextPageButton.active =
        this.skinStartIndex + 1 + this.maxSkinsPerPage < this.numOfSkins;
  }

  @Override
  public void init() {
    super.init();

    // Cache
    if (this.entity != null) {
      this.professions = this.entity.getProfessions();
      this.variants = this.entity.getVariants();
      this.numOfProfessions = this.entity.hasProfessions() ? this.professions.length : 0;
      this.numOfVariants = this.variants.length;
      this.numOfSkins =
          numOfProfessions > 0 ? this.numOfVariants * this.numOfProfessions : this.numOfVariants;
      log.debug("Found about {} skins with {} variants and {} professions.", this.numOfSkins,
          this.numOfVariants, this.numOfProfessions);
    }

    // Default stats
    this.imageHeight = 220;
    this.imageWidth = 275;

    // Basic Position
    this.titleLabelX = 60;
    this.titleLabelY = 6;
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Name
    this.nameBox = new EditBox(this.font, this.leftPos + 7, this.topPos + 31, 180, 18,
        new TranslatableComponent("Name"));
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(
        this.entity.hasCustomName() ? this.entity.getCustomName().getString() : "My Easy NPC");
    this.addRenderableWidget(this.nameBox);

    this.saveNameButton = this.addRenderableWidget(new Button(this.leftPos + 190, this.topPos + 30,
        80, 20, new TranslatableComponent("Save"), onPress -> {
          this.saveName();
        }));

    // Dialog
    this.editDialogButton = this.addRenderableWidget(new Button(this.leftPos + 10, this.topPos + 54,
        80, 20, new TranslatableComponent("Edit Dialog"), onPress -> {
          log.info("Edit dialog ...");
          DialogType dialogType = this.entity.getDialogType();
          switch (dialogType) {
            case BASIC:
              NetworkHandler.openDialog(uuid, "BasicDialogConfiguration");
              break;
            case YES_NO:
              NetworkHandler.openDialog(uuid, "YesNoDialogConfiguration");
              break;
            default:
              NetworkHandler.openDialog(uuid, "BasicDialogConfiguration");
          }
        }));

    // Skins
    int skinButtonTop = this.topPos + 110;
    int skinButtonLeft = this.leftPos + 7;
    int skinButtonRight = this.leftPos + 255;
    this.skinPreviousButton = this.addRenderableWidget(new Button(skinButtonLeft, skinButtonTop, 15,
        20, new TranslatableComponent("<"), onPress -> {
          if (this.skinStartIndex > 0) {
            skinStartIndex--;
          }
          checkSkinButtonState();
        }));
    this.skinPreviousPageButton = this.addRenderableWidget(new Button(skinButtonLeft,
        skinButtonTop + 20, 15, 20, new TranslatableComponent("<<"), onPress -> {
          if (this.skinStartIndex - maxSkinsPerPage > 0) {
            skinStartIndex = skinStartIndex - maxSkinsPerPage;
          } else {
            skinStartIndex = 0;
          }
          checkSkinButtonState();
        }));
    this.skinNextButton = this.addRenderableWidget(new Button(skinButtonRight, skinButtonTop, 15,
        20, new TranslatableComponent(">"), onPress -> {
          if (this.skinStartIndex >= 0
              && this.skinStartIndex < this.numOfSkins - this.maxSkinsPerPage) {
            skinStartIndex++;
          }
          checkSkinButtonState();
        }));
    this.skinNextPageButton = this.addRenderableWidget(new Button(skinButtonRight,
        skinButtonTop + 20, 15, 20, new TranslatableComponent(">>"), onPress -> {
          if (this.skinStartIndex >= 0
              && this.skinStartIndex + this.maxSkinsPerPage < this.numOfSkins) {
            this.skinStartIndex = this.skinStartIndex + this.maxSkinsPerPage;
          } else if (this.numOfSkins > this.maxSkinsPerPage) {
            this.skinStartIndex = this.numOfSkins - this.maxSkinsPerPage;
          } else {
            this.skinStartIndex = this.numOfSkins;
          }
          checkSkinButtonState();
        }));
    checkSkinButtonState();

    // Actions
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Name
    this.font.draw(poseStack, new TextComponent("Name"), this.leftPos + 7f, this.topPos + 20f,
        4210752);

    // Skins
    this.font.draw(poseStack, new TextComponent("Skins"), this.leftPos + 7f, this.topPos + 80f,
        4210752);
    this.renderSkins(poseStack, partialTicks);
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    this.font.draw(poseStack, this.title, this.titleLabelX, this.titleLabelY, 4210752);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen (+50px in height)
    this.blit(poseStack, leftPos, topPos, 0, 0, 250, 170);
    this.blit(poseStack, leftPos + 243, topPos, 215, 0, 35, 170);

    int expandedHeight = 50;
    this.blit(poseStack, leftPos, topPos + expandedHeight + 5, 0, 5, 250, 170);
    this.blit(poseStack, leftPos + 243, topPos + expandedHeight + 5, 215, 5, 35, 170);

    // Skin Selection
    fill(poseStack, this.leftPos + 7, this.topPos + 90, this.leftPos + 269, this.topPos + 180,
        0xff000000);
    fill(poseStack, this.leftPos + 8, this.topPos + 91, this.leftPos + 268, this.topPos + 179,
        0xffaaaaaa);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69) {
      return super.keyPressed(keyCode, unused1, unused2);
    } else if (keyCode == 257 || keyCode == 335) {
      return true;
    } else {
      return true;
    }
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    // Make sure we pass the mouse click to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.mouseClicked(mouseX, mouseY, button);
      }
    }
    return super.mouseClicked(mouseX, mouseY, button);
  }

}
