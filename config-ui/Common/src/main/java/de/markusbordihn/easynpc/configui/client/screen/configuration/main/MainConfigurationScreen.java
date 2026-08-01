/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen.configuration.main;

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.client.screen.EntityGuiScaling;
import de.markusbordihn.easynpc.configui.client.screen.ExperimentalFeaturesState;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.ColorButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ColorPickerPopup;
import de.markusbordihn.easynpc.configui.client.screen.components.CopyButton;
import de.markusbordihn.easynpc.configui.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ExperimentalButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ExportButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ImportButton;
import de.markusbordihn.easynpc.configui.client.screen.components.NameVisibilityToggleButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ReloadButton;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProgressionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.client.input.CharacterEvent;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.contents.TranslatableContents;
import net.minecraft.world.entity.player.Inventory;

public class MainConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  public static final int BUTTON_HEIGHT = 18;
  public static final int BUTTON_WIDTH = 97;
  private static final float ENTITY_TEXT_SCALE = 0.75f;
  private static final Map<String, ConfigurationType> menuButtons = new LinkedHashMap<>();

  static {
    menuButtons.put("actions", ConfigurationType.BASIC_ACTION);
    menuButtons.put("attributes", ConfigurationType.ABILITIES_ATTRIBUTE);
    menuButtons.put("dialog", ConfigurationType.DIALOG);
    menuButtons.put("equipment", ConfigurationType.EQUIPMENT);
    menuButtons.put("objective", ConfigurationType.BASIC_OBJECTIVE);
    menuButtons.put("pose", ConfigurationType.POSE);
    menuButtons.put("position", ConfigurationType.DEFAULT_POSITION);
    menuButtons.put("rotation", ConfigurationType.DEFAULT_ROTATION);
    menuButtons.put("scaling", ConfigurationType.SCALING);
    menuButtons.put("trading", ConfigurationType.TRADING);
  }

  private Button copyUUIDButton;
  private String formerName = "";
  private int formerTextColor = 0;
  private NameVisibilityType formerNameVisibility = NameVisibilityType.ALWAYS;
  private EditBox nameBox;
  private ColorButton nameColorButton;
  private ColorPickerPopup colorPickerPopup;
  private NameVisibilityToggleButton nameVisibilityButton;
  private Button saveNameButton;
  private int avatarTopPos;
  private int avatarHeight;

  public MainConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.showCloseButton = true;
  }

  private static String formatPosition(BlockPos blockPos) {
    return blockPos.getX() + ", " + blockPos.getY() + ", " + blockPos.getZ();
  }

  @Override
  public void init() {
    super.init();

    // Core Positions
    this.contentTopPos = this.topPos + 15;
    this.avatarTopPos = this.contentTopPos + 1;
    this.avatarHeight = 170;

    // Hide home button
    this.homeButton.visible = false;

    // Define buttons and boxes
    this.defineNameAndColorBox();
    this.defineImportExportButtons();
    this.defineUUIDButton();
    this.defineRespawnButton();
    this.defineDeleteButton();
    this.defineEditSkinButton();
    this.defineChangeModelButton();
    this.defineMenuButtons();
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    // While the color picker popup covers the pointer, report the pointer off-screen to the
    // underlying screen so the widgets below the popup do not render a hover highlight.
    if (this.colorPickerPopup != null && this.colorPickerPopup.isMouseOver(x, y)) {
      super.extractRenderState(guiGraphics, -1, -1, partialTicks);
      this.xMouse = x;
      this.yMouse = y;
    } else {
      super.extractRenderState(guiGraphics, x, y, partialTicks);
    }

    if (getEasyNPC() == null) {
      return;
    }

    IntegrationRegistry.setGuiPreviewMode(true);
    // Render Entity
    EntityConfigScreenRenderer.renderEntityRaw(
        guiGraphics,
        getEasyNPC(),
        EntityRenderConfig.guiScaled(
            this.leftPos + 60, this.avatarTopPos + 97, EntityGuiScaling.getScaling(getEasyNPC())),
        this.xMouse,
        this.yMouse);
    IntegrationRegistry.setGuiPreviewMode(false);

    guiGraphics.pose().pushMatrix();
    guiGraphics.pose().scale(ENTITY_TEXT_SCALE, ENTITY_TEXT_SCALE);

    Text.drawString(
        guiGraphics,
        this.font,
        "UUID: " + this.getEasyNPCEntity().getUUID(),
        Math.round((this.contentLeftPos + 1) / ENTITY_TEXT_SCALE),
        Math.round((this.buttonTopPos + 1) / ENTITY_TEXT_SCALE));
    Text.drawString(
        guiGraphics,
        this.font,
        this.getEasyNPCEntity().getType().getDescription(),
        Math.round((this.contentLeftPos + 3) / ENTITY_TEXT_SCALE),
        Math.round((this.avatarTopPos + 4) / ENTITY_TEXT_SCALE));

    OwnerDataCapable<?> ownerData = this.getEasyNPC().getEasyNPCOwnerData();
    if (ownerData != null) {
      this.drawAvatarInfo(
          guiGraphics,
          15,
          "Owner: " + (ownerData.hasNPCOwner() ? ownerData.getNPCOwnerName() : "-"));
    }

    NavigationDataCapable<?> navigationData = this.getEasyNPC().getEasyNPCNavigationData();
    if (navigationData != null && navigationData.hasNPCHomePosition()) {
      this.drawAvatarInfo(
          guiGraphics, 23, "Home: " + formatPosition(navigationData.getNPCHomePosition()));
    }

    this.drawAvatarInfo(
        guiGraphics,
        31,
        "Team: "
            + (this.getEasyNPCEntity().getTeam() != null
                ? this.getEasyNPCEntity().getTeam().getName()
                : "-"));
    this.drawAvatarInfo(
        guiGraphics,
        39,
        "HP: "
            + this.getEasyNPCLivingEntity().getHealth()
            + "/"
            + this.getEasyNPCLivingEntity().getMaxHealth());

    ProgressionDataCapable<?> progressionData = this.getEasyNPC().getEasyNPCProgressionData();
    if (progressionData != null && progressionData.getExperience() > 1) {
      this.drawAvatarInfo(
          guiGraphics,
          47,
          "Level: "
              + progressionData.getExperienceLevel()
              + " (XP: "
              + progressionData.getExperience()
              + "/"
              + progressionData.getExperienceForNextLevel()
              + ")");
    }

    this.drawAvatarInfo(
        guiGraphics,
        this.avatarHeight - 8,
        "Pos: " + formatPosition(this.getEasyNPCEntity().getOnPos()));

    guiGraphics.pose().popMatrix();

    if (this.colorPickerPopup != null) {
      this.colorPickerPopup.render(guiGraphics, x, y, partialTicks);
    }
  }

  private void drawAvatarInfo(GuiGraphicsExtractor guiGraphics, int topOffset, String text) {
    Text.drawString(
        guiGraphics,
        this.font,
        text,
        Math.round((this.contentLeftPos + 3) / ENTITY_TEXT_SCALE),
        Math.round((this.avatarTopPos + topOffset) / ENTITY_TEXT_SCALE));
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (this.colorPickerPopup != null
        && this.colorPickerPopup.mouseClicked(mouseButtonEvent, doubleClick)) {
      return true;
    }
    return super.mouseClicked(mouseButtonEvent, doubleClick);
  }

  @Override
  public boolean keyPressed(KeyEvent keyEvent) {
    if (this.colorPickerPopup != null
        && this.colorPickerPopup.isVisible()
        && this.colorPickerPopup.keyPressed(keyEvent)) {
      return true;
    }
    return super.keyPressed(keyEvent);
  }

  @Override
  public boolean charTyped(CharacterEvent characterEvent) {
    if (this.colorPickerPopup != null
        && this.colorPickerPopup.isVisible()
        && this.colorPickerPopup.charTyped(characterEvent)) {
      return true;
    }
    return super.charTyped(characterEvent);
  }

  @Override
  public void extractBackground(
      GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
    super.extractBackground(guiGraphics, mouseX, mouseY, partialTicks);

    // Entity Type
    guiGraphics.fill(
        this.contentLeftPos,
        this.avatarTopPos,
        this.leftPos + 117,
        this.avatarTopPos + 135,
        0xff000000);
    guiGraphics.fill(
        this.leftPos + 8,
        this.avatarTopPos + 1,
        this.leftPos + 116,
        this.avatarTopPos + 134,
        0xffffffff);

    // Entity
    guiGraphics.fill(
        this.contentLeftPos,
        this.avatarTopPos + 12,
        this.leftPos + 117,
        this.avatarTopPos + this.avatarHeight,
        0xff000000);
    guiGraphics.fill(
        this.leftPos + 8,
        this.avatarTopPos + 13,
        this.leftPos + 116,
        this.avatarTopPos + this.avatarHeight,
        0xffaaaaaa);
  }

  private void openConfiguration(ConfigurationType configurationType) {
    NetworkMessageHandlerManager.getServerHandler()
        .openConfiguration(this.getEasyNPCUUID(), configurationType);
  }

  private void defineImportExportButtons() {
    // Import Button — opens local import screen (always accessible)
    Button importButton =
        this.addRenderableWidget(
            new ImportButton(
                this.leftPos + 122,
                this.contentTopPos + 35,
                97,
                16,
                "import",
                onPress -> this.openConfiguration(ConfigurationType.LOCAL_PRESET_IMPORT)));

    // Export Button
    Button exportButton =
        this.addRenderableWidget(
            new ExportButton(
                importButton.getX() + importButton.getWidth() + 5,
                importButton.getY(),
                97,
                16,
                "export",
                onPress -> this.openConfiguration(ConfigurationType.LOCAL_PRESET_EXPORT)));
    if (this.isConfigurationBlockedByPermission(ConfigurationType.LOCAL_PRESET_EXPORT)) {
      exportButton.active = false;
      exportButton.setTooltip(
          Tooltip.create(TextComponent.getTranslatedConfigText("menu.tooltip.no_permission")));
    } else {
      exportButton.active = true;
    }
  }

  private void defineNameAndColorBox() {
    // Name Edit Box
    Component nameComponent = getEasyNPCEntity().getName();
    this.formerName =
        nameComponent.getContents() instanceof TranslatableContents translatableContents
            ? translatableContents.getKey()
            : nameComponent.getString();
    this.nameBox = new TextField(this.font, this.contentLeftPos + 115, this.contentTopPos + 2, 138);
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(this.formerName);
    this.nameBox.setResponder(consumer -> this.validateName());
    this.addRenderableWidget(this.nameBox);

    // Color Picker Popup
    this.colorPickerPopup =
        new ColorPickerPopup(
            this.font,
            selectedColor -> {
              this.nameColorButton.setColorValue(selectedColor);
              this.validateName();
            });

    // Name Color Button
    this.nameColorButton =
        this.addRenderableWidget(
            new ColorButton(
                this.nameBox.getX() + this.nameBox.getWidth() + 1,
                this.nameBox.getY() - 1,
                onPress ->
                    this.colorPickerPopup.toggle(
                        this.nameColorButton.getColorValue(),
                        this.nameColorButton.getX(),
                        this.nameColorButton.getY() + this.nameColorButton.getHeight() + 1,
                        this.width,
                        this.height)));
    if (getEasyNPCEntity().hasCustomName()
        && getEasyNPCEntity().getCustomName().getStyle() != null
        && getEasyNPCEntity().getCustomName().getStyle().getColor() != null) {
      int styleTextColor = getEasyNPCEntity().getCustomName().getStyle().getColor().getValue();
      this.nameColorButton.setColorValue(styleTextColor);
      this.formerTextColor = styleTextColor;
    }

    // Name Visibility Button
    DisplayAttributeDataCapable<?> displayAttributeData =
        getEasyNPC().getEasyNPCDisplayAttributeData();
    NameVisibilityType currentVisibility =
        displayAttributeData != null
            ? displayAttributeData.getDisplayEnumAttribute(
                DisplayAttributeType.NAME_VISIBILITY, NameVisibilityType.class)
            : NameVisibilityType.ALWAYS;
    this.nameVisibilityButton =
        this.addRenderableWidget(
            new NameVisibilityToggleButton(
                this.nameColorButton.getX() + this.nameColorButton.getWidth() + 2,
                this.nameColorButton.getY(),
                currentVisibility,
                (button, newType) -> this.validateName()));
    this.formerNameVisibility = currentVisibility;

    // Save Name Button
    this.saveNameButton =
        this.addRenderableWidget(
            new SaveButton(
                this.nameVisibilityButton.getX() + this.nameVisibilityButton.getWidth() + 2,
                this.nameVisibilityButton.getY(),
                onPress -> this.saveName()));
    this.saveNameButton.active = false;
  }

  private void defineUUIDButton() {
    this.copyUUIDButton =
        this.addRenderableWidget(
            new CopyButton(
                this.contentLeftPos,
                this.bottomPos - 27,
                90,
                18,
                "copy_uuid",
                onPress -> {
                  Minecraft minecraft = Minecraft.getInstance();
                  minecraft.keyboardHandler.setClipboard(this.getEasyNPCUUID().toString());
                }));
  }

  private void defineRespawnButton() {
    this.addRenderableWidget(
        new ReloadButton(
            copyUUIDButton.getX() + copyUUIDButton.getWidth() + 4,
            this.bottomPos - 27,
            80,
            18,
            "respawn",
            onPress -> respawnNPC()));
  }

  private void defineDeleteButton() {
    this.addRenderableWidget(
        new DeleteButton(
            this.rightPos - 70, this.bottomPos - 27, 66, 18, onPress -> this.deleteNPC()));
  }

  protected void defineEditSkinButton() {
    SkinDataCapable<?> skinData = this.getEasyNPC().getEasyNPCSkinData();
    if (skinData == null) {
      return;
    }

    Button editSkinButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.avatarTopPos + this.avatarHeight,
                110,
                14,
                "edit_skin",
                onPress ->
                    this.openConfiguration(
                        switch (skinData.getSkinType()) {
                          case NONE -> ConfigurationType.NONE_SKIN;
                          case PLAYER_SKIN -> ConfigurationType.PLAYER_SKIN;
                          case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> ConfigurationType.URL_SKIN;
                          case CUSTOM -> ConfigurationType.CUSTOM_SKIN;
                          default -> ConfigurationType.DEFAULT_SKIN;
                        })));
    editSkinButton.active = this.supportsConfigurationType(ConfigurationType.SKIN);
  }

  protected void defineChangeModelButton() {
    RenderDataEntry renderDataSet = this.getRenderDataEntry();
    if (renderDataSet == null) {
      return;
    }

    Button changeModelButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.avatarTopPos + this.avatarHeight + 14,
                110,
                14,
                "change_model",
                onPress -> {
                  if (this.supportsConfigurationType(ConfigurationType.COBBLEMON_MODEL)) {
                    this.openConfiguration(ConfigurationType.COBBLEMON_MODEL);
                  } else if (this.supportsConfigurationType(
                      ConfigurationType.EASY_MODEL_ENTITIES_MODEL)) {
                    this.openConfiguration(ConfigurationType.EASY_MODEL_ENTITIES_MODEL);
                  } else if (renderDataSet.getRenderType() == RenderType.CUSTOM_ENTITY) {
                    this.openConfiguration(ConfigurationType.CUSTOM_MODEL);
                  } else {
                    this.openConfiguration(ConfigurationType.DEFAULT_MODEL);
                  }
                }));
    changeModelButton.active =
        this.supportsConfigurationType(ConfigurationType.CUSTOM_MODEL)
            || this.supportsConfigurationType(ConfigurationType.COBBLEMON_MODEL)
            || this.supportsConfigurationType(ConfigurationType.EASY_MODEL_ENTITIES_MODEL);
    if (!changeModelButton.active) {
      changeModelButton.setTooltip(
          Tooltip.create(
              TextComponent.getTranslatedConfigText("change_model.tooltip.only_doppler")));
    }
  }

  protected void defineMenuButtons() {
    int buttonTopPos = this.topPos + 75;
    int buttonLeftPos = this.contentLeftPos + 115;
    int buttonIndex = 0;
    List<Button> experimentalButtons = new ArrayList<>();

    for (Map.Entry<String, ConfigurationType> entry : menuButtons.entrySet()) {
      String buttonName = entry.getKey();
      ConfigurationType configurationType = entry.getValue();
      boolean experimental = this.isExperimentalConfigurationType(configurationType);
      boolean permissionBlocked = this.isConfigurationBlockedByPermission(configurationType);
      int buttonX = buttonLeftPos + ((buttonIndex % 2) * (BUTTON_WIDTH + 5));
      int buttonY = buttonTopPos + ((buttonIndex / 2) * (BUTTON_HEIGHT + 2));

      Button button;
      if (experimental) {
        button =
            this.addRenderableWidget(
                new ExperimentalButton(
                    buttonX,
                    buttonY,
                    BUTTON_WIDTH,
                    buttonName,
                    onPress -> this.openConfiguration(configurationType)));
        button.active = ExperimentalFeaturesState.isEnabled() && !permissionBlocked;
        experimentalButtons.add(button);
      } else {
        button =
            this.addRenderableWidget(
                new TextButton(
                    buttonX,
                    buttonY,
                    BUTTON_WIDTH,
                    buttonName,
                    onPress -> this.openConfiguration(configurationType)));
        boolean typeSupported = this.supportsConfigurationType(configurationType);
        button.active = typeSupported && !permissionBlocked;
        if (typeSupported && permissionBlocked) {
          button.setTooltip(
              Tooltip.create(TextComponent.getTranslatedConfigText("menu.tooltip.no_permission")));
        }
      }

      buttonIndex++;
    }

    if (!experimentalButtons.isEmpty()) {
      int rows = (menuButtons.size() + 1) / 2;
      int checkboxY = buttonTopPos + (rows * (BUTTON_HEIGHT + 2)) + 2;
      this.addRenderableWidget(
          new Checkbox(
              buttonLeftPos,
              checkboxY,
              TextComponent.getTranslatedConfigText("experimental_features"),
              ExperimentalFeaturesState.isEnabled(),
              true,
              checkbox -> {
                ExperimentalFeaturesState.setEnabled(checkbox.selected());
                for (Button experimentalButton : experimentalButtons) {
                  experimentalButton.active = checkbox.selected();
                }
              }));
    }
  }

  private void respawnNPC() {
    this.confirmAction(
        "respawnNPC.confirmQuestion",
        "respawnNPC.confirmWarning",
        "respawnNPC.respawnButton",
        () -> NetworkMessageHandlerManager.getServerHandler().respawnNPC(this.getEasyNPCUUID()));
  }

  private void deleteNPC() {
    this.confirmAction(
        "removeNPC.deleteQuestion",
        "removeNPC.deleteWarning",
        "removeNPC.deleteButton",
        () -> NetworkMessageHandlerManager.getServerHandler().removeNPC(this.getEasyNPCUUID()));
  }

  private void confirmAction(
      String questionKey, String warningKey, String confirmKey, Runnable confirmedAction) {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }

    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed) {
                confirmedAction.run();
                minecraft.setScreen(null);
              } else {
                minecraft.setScreen(this);
              }
            },
            TextComponent.getTranslatedConfigText(questionKey),
            TextComponent.getTranslatedConfigText(
                warningKey, this.getEasyNPCEntity().getDisplayName()),
            TextComponent.getTranslatedConfigText(confirmKey),
            CommonComponents.GUI_CANCEL));
  }

  private void validateName() {
    String nameValue = this.nameBox.getValue();
    int textColor = this.nameColorButton.getColorValue();
    NameVisibilityType nameVisibility = this.nameVisibilityButton.getVisibilityType();
    this.saveNameButton.active =
        !this.formerName.equals(nameValue)
            || this.formerTextColor != textColor
            || this.formerNameVisibility != nameVisibility;
  }

  private void saveName() {
    String name = this.nameBox.getValue();
    int textColor = this.nameColorButton.getColorValue();
    NameVisibilityType nameVisibility = this.nameVisibilityButton.getVisibilityType();
    NetworkMessageHandlerManager.getServerHandler()
        .changeName(getEasyNPC().getEntityUUID(), name, textColor, nameVisibility);
    this.formerName = name;
    this.formerTextColor = textColor;
    this.formerNameVisibility = nameVisibility;
    this.saveNameButton.active = false;
  }
}
