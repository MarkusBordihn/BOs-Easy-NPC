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

package de.markusbordihn.easynpc.entity;

import java.util.Optional;
import java.util.UUID;

import javax.annotation.Nullable;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.SlotAccess;
import net.minecraft.world.entity.npc.InventoryCarrier;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.skin.SkinModel;
import de.markusbordihn.easynpc.skin.SkinType;
import de.markusbordihn.easynpc.utils.TextUtils;

public class EasyNPCEntityData extends AgeableMob implements InventoryCarrier, Npc {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Default values
  private ResourceLocation baseTextureLocation;
  private ResourceLocation professionTextureLocation;
  private ResourceLocation textureLocation;
  private boolean hasBaseTextureLocation = false;
  private boolean hasProfessionTextureLocation = false;
  private boolean hasTextureLocation = false;

  // Default Variants
  private enum Variant {
    STEVE
  }

  // Synced Data
  private static final EntityDataAccessor<Optional<UUID>> DATA_OWNER_UUID_ID =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.OPTIONAL_UUID);
  private static final EntityDataAccessor<String> DATA_DIALOG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_DIALOG_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_NO_DIALOG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_NO_DIALOG_BUTTON =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_PROFESSION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_SKIN =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_SKIN_URL =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<Optional<UUID>> DATA_SKIN_UUID =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.OPTIONAL_UUID);
  private static final EntityDataAccessor<String> DATA_SKIN_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_VARIANT =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_YES_DIALOG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_YES_DIALOG_BUTTON =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);

  // Stored Entity Data Tags
  private static final String DATA_DIALOG_TAG = "Dialog";
  private static final String DATA_DIALOG_TYPE_TAG = "DialogType";
  private static final String DATA_INVENTORY_TAG = "Inventory";
  private static final String DATA_NO_DIALOG_BUTTON_TAG = "NoDialogButton";
  private static final String DATA_NO_DIALOG_TAG = "NoDialog";
  private static final String DATA_OWNER_TAG = "Owner";
  private static final String DATA_PROFESSION_TAG = "Profession";
  private static final String DATA_SKIN_TAG = "Skin";
  private static final String DATA_SKIN_URL_TAG = "SkinURL";
  private static final String DATA_SKIN_UUID_TAG = "SkinUUID";
  private static final String DATA_SKIN_TYPE_TAG = "SkinType";
  private static final String DATA_VARIANT_TAG = "Variant";
  private static final String DATA_YES_DIALOG_BUTTON_TAG = "YesDialogButton";
  private static final String DATA_YES_DIALOG_TAG = "YesDialog";

  // Inventory
  private final SimpleContainer inventory = new SimpleContainer(8);

  public EasyNPCEntityData(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
  }

  public boolean hasBaseTextureLocation() {
    return this.hasBaseTextureLocation;
  }

  public ResourceLocation getBaseTextureLocation() {
    return this.baseTextureLocation;
  }

  public void setBaseTextureLocation(ResourceLocation textureLocation) {
    this.baseTextureLocation = textureLocation;
    this.hasBaseTextureLocation = this.baseTextureLocation != null;
  }

  public boolean hasTextureLocation() {
    return this.hasTextureLocation;
  }

  public ResourceLocation getTextureLocation() {
    return this.textureLocation;
  }

  public void setTextureLocation(ResourceLocation textureLocation) {
    this.textureLocation = textureLocation;
    this.hasTextureLocation = textureLocation != null;
  }

  public boolean hasProfessionTextureLocation() {
    return this.hasProfessionTextureLocation;
  }

  public ResourceLocation getProfessionTextureLocation() {
    return this.professionTextureLocation;
  }

  public void setProfessionTextureLocation(ResourceLocation textureLocation) {
    this.professionTextureLocation = textureLocation;
    this.hasProfessionTextureLocation = textureLocation != null;
  }

  public DialogType getDialogType() {
    return DialogType.get(this.entityData.get(DATA_DIALOG_TYPE));
  }

  public void setDialogType(DialogType dialogType) {
    this.entityData.set(DATA_DIALOG_TYPE, dialogType != null ? dialogType.name() : "");
  }

  public String getDialog() {
    return this.entityData.get(DATA_DIALOG);
  }

  public void setDialog(String dialog) {
    this.entityData.set(DATA_DIALOG, dialog);
  }

  public String getNoDialog() {
    return this.entityData.get(DATA_NO_DIALOG);
  }

  public void setNoDialog(String dialog) {
    this.entityData.set(DATA_NO_DIALOG, dialog);
  }

  public String getNoDialogButton() {
    return this.entityData.get(DATA_NO_DIALOG_BUTTON);
  }

  public void setNoDialogButton(String dialogButton) {
    this.entityData.set(DATA_NO_DIALOG_BUTTON, dialogButton);
  }

  public String getYesDialog() {
    return this.entityData.get(DATA_YES_DIALOG);
  }

  public void setYesDialog(String dialog) {
    this.entityData.set(DATA_YES_DIALOG, dialog);
  }

  public String getYesDialogButton() {
    return this.entityData.get(DATA_YES_DIALOG_BUTTON);
  }

  public void setYesDialogButton(String dialogButton) {
    this.entityData.set(DATA_YES_DIALOG_BUTTON, dialogButton);
  }

  public Enum<?> getDefaultProfession() {
    return Profession.NONE;
  }

  public Enum<?> getProfession() {
    return getProfession(this.entityData.get(DATA_PROFESSION));
  }

  public Enum<?> getProfession(String name) {
    return Profession.valueOf(name);
  }

  public void setProfession(Enum<?> profession) {
    this.entityData.set(DATA_PROFESSION, profession != null ? profession.name() : "");
  }

  public void setProfession(String name) {
    Enum<?> profession = getProfession(name);
    if (profession != null) {
      setProfession(profession);
    } else {
      log.error("Unknown profession {} for {}", name, this);
    }
  }

  public boolean hasProfessions() {
    return false;
  }

  public Enum<?>[] getProfessions() {
    return Profession.values();
  }

  public boolean hasProfession() {
    return false;
  }

  public Component getProfessionName() {
    Enum<?> profession = getProfession();
    return profession != null ? TextUtils.normalizeName(profession.name()) : Component.literal("");
  }

  public String getSkin() {
    return this.entityData.get(DATA_SKIN);
  }

  public void setSkin(String skin) {
    this.entityData.set(DATA_SKIN, skin != null ? skin : "");
  }

  public String getSkinURL() {
    return this.entityData.get(DATA_SKIN_URL);
  }

  public void setSkinURL(String skinURL) {
    this.entityData.set(DATA_SKIN_URL, skinURL != null ? skinURL : "");
  }

  public Optional<UUID> getSkinUUID() {
    return this.entityData.get(DATA_SKIN_UUID);
  }

  public void setSkinUUID(UUID uuid) {
    this.entityData.set(DATA_SKIN_UUID, Optional.of(uuid));
  }

  public void setSkinUUID(Optional<UUID> uuid) {
    this.entityData.set(DATA_SKIN_UUID, uuid);
  }

  public SkinType getSkinType() {
    return getSkinType(this.entityData.get(DATA_SKIN_TYPE));
  }

  public SkinType getSkinType(String name) {
    return SkinType.get(name);
  }

  public void setSkinType(SkinType skinType) {
    this.entityData.set(DATA_SKIN_TYPE, skinType != null ? skinType.name() : "");
  }

  public void setSkinType(String name) {
    SkinType skinType = getSkinType(name);
    if (skinType != null) {
      setSkinType(skinType);
    } else {
      log.error("Unknown skin type {} for {}", name, this);
    }
  }

  public SkinModel getSkinModel() {
    return SkinModel.HUMANOID;
  }

  public Enum<?> getDefaultVariant() {
    return Variant.STEVE;
  }

  public Enum<?> getVariant() {
    return getVariant(this.entityData.get(DATA_VARIANT));
  }

  public Enum<?> getVariant(String name) {
    return Variant.valueOf(name);
  }

  public void setVariant(Enum<?> variant) {
    this.entityData.set(DATA_VARIANT, variant != null ? variant.name() : "");
  }

  public void setVariant(String name) {
    Enum<?> variant = getVariant(name);
    if (variant != null) {
      setVariant(variant);
    } else {
      log.error("Unknown variant {} for {}", name, this);
    }
  }

  public Enum<?>[] getVariants() {
    return Variant.values();
  }

  public SimpleContainer getInventory() {
    return this.inventory;
  }

  public Component getVariantName() {
    Enum<?> variant = getVariant();
    return variant != null ? TextUtils.normalizeName(variant.name()) : this.getTypeName();
  }

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    return component != null ? TextUtils.removeAction(component) : this.getTypeName();
  }

  @Override
  public SlotAccess getSlot(int slotIndex) {
    int i = slotIndex - 300;
    return i >= 0 && i < this.inventory.getContainerSize()
        ? SlotAccess.forContainer(this.inventory, i)
        : super.getSlot(slotIndex);
  }

  @Nullable
  public UUID getOwnerUUID() {
    return this.entityData.get(DATA_OWNER_UUID_ID).orElse((UUID) null);
  }

  public void setOwnerUUID(@Nullable UUID uuid) {
    this.entityData.set(DATA_OWNER_UUID_ID, Optional.ofNullable(uuid));
  }

  public boolean hasOwner() {
    return this.getOwnerUUID() != null;
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();
    this.entityData.define(DATA_DIALOG, "");
    this.entityData.define(DATA_DIALOG_TYPE, DialogType.NONE.name());
    this.entityData.define(DATA_NO_DIALOG, "");
    this.entityData.define(DATA_NO_DIALOG_BUTTON, "No");
    this.entityData.define(DATA_OWNER_UUID_ID, Optional.empty());
    this.entityData.define(DATA_PROFESSION, this.getDefaultProfession().name());
    this.entityData.define(DATA_SKIN, "");
    this.entityData.define(DATA_SKIN_URL, "");
    this.entityData.define(DATA_SKIN_UUID, Optional.empty());
    this.entityData.define(DATA_SKIN_TYPE, SkinType.DEFAULT.name());
    this.entityData.define(DATA_VARIANT, this.getDefaultVariant().name());
    this.entityData.define(DATA_YES_DIALOG, "");
    this.entityData.define(DATA_YES_DIALOG_BUTTON, "Yes");
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    if (this.getDialogType() != null) {
      compoundTag.putString(DATA_DIALOG_TYPE_TAG, this.getDialogType().name());
    }
    if (this.getDialog() != null) {
      compoundTag.putString(DATA_DIALOG_TAG, this.getDialog());
    }
    if (this.getNoDialog() != null) {
      compoundTag.putString(DATA_NO_DIALOG_TAG, this.getNoDialog());
    }
    if (this.getNoDialogButton() != null) {
      compoundTag.putString(DATA_NO_DIALOG_BUTTON_TAG, this.getNoDialogButton());
    }
    if (this.getYesDialog() != null) {
      compoundTag.putString(DATA_YES_DIALOG_TAG, this.getYesDialog());
    }
    if (this.getYesDialogButton() != null) {
      compoundTag.putString(DATA_YES_DIALOG_BUTTON_TAG, this.getYesDialogButton());
    }
    if (this.getOwnerUUID() != null) {
      compoundTag.putUUID(DATA_OWNER_TAG, this.getOwnerUUID());
    }
    if (this.getProfession() != null) {
      compoundTag.putString(DATA_PROFESSION_TAG, this.getProfession().name());
    }
    if (this.getSkin() != null) {
      compoundTag.putString(DATA_SKIN_TAG, this.getSkin());
    }
    if (this.getSkinURL() != null) {
      compoundTag.putString(DATA_SKIN_URL_TAG, this.getSkinURL());
    }
    Optional<UUID> skinUUID = this.getSkinUUID();
    if (skinUUID.isPresent()) {
      compoundTag.putUUID(DATA_SKIN_UUID_TAG, skinUUID.get());
    }
    if (this.getSkinType() != null) {
      compoundTag.putString(DATA_SKIN_TYPE_TAG, this.getSkinType().name());
    }
    if (this.getVariant() != null) {
      compoundTag.putString(DATA_VARIANT_TAG, this.getVariant().name());
    }
    compoundTag.put(DATA_INVENTORY_TAG, this.inventory.createTag());
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);

    if (compoundTag.contains(DATA_DIALOG_TYPE_TAG)) {
      String dialogType = compoundTag.getString(DATA_DIALOG_TYPE_TAG);
      if (dialogType != null && !dialogType.isEmpty()) {
        this.setDialogType(DialogType.get(dialogType));
      }
    }
    if (compoundTag.contains(DATA_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_DIALOG_TAG);
      if (dialog != null) {
        this.setDialog(dialog);
      }
    }
    if (compoundTag.contains(DATA_NO_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_NO_DIALOG_TAG);
      if (dialog != null) {
        this.setNoDialog(dialog);
      }
    }
    if (compoundTag.contains(DATA_NO_DIALOG_BUTTON_TAG)) {
      String dialogButton = compoundTag.getString(DATA_NO_DIALOG_BUTTON_TAG);
      if (dialogButton != null) {
        this.setNoDialogButton(dialogButton);
      }
    }
    if (compoundTag.contains(DATA_YES_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_YES_DIALOG_TAG);
      if (dialog != null) {
        this.setYesDialog(dialog);
      }
    }
    if (compoundTag.contains(DATA_YES_DIALOG_BUTTON_TAG)) {
      String dialogButton = compoundTag.getString(DATA_YES_DIALOG_BUTTON_TAG);
      if (dialogButton != null) {
        this.setYesDialogButton(dialogButton);
      }
    }
    if (compoundTag.hasUUID(DATA_OWNER_TAG)) {
      UUID uuid = compoundTag.getUUID(DATA_OWNER_TAG);
      if (uuid != null) {
        this.setOwnerUUID(uuid);
      }
    }
    if (compoundTag.contains(DATA_PROFESSION_TAG)) {
      String profession = compoundTag.getString(DATA_PROFESSION_TAG);
      if (profession != null && !profession.isEmpty()) {
        this.setProfession(this.getProfession(profession));
      }
    }
    if (compoundTag.contains(DATA_VARIANT_TAG)) {
      String variant = compoundTag.getString(DATA_VARIANT_TAG);
      if (variant != null && !variant.isEmpty()) {
        this.setVariant(this.getVariant(variant));
      }
    }
    if (compoundTag.contains(DATA_SKIN_TAG)) {
      String skin = compoundTag.getString(DATA_SKIN_TAG);
      if (skin != null && !skin.isEmpty()) {
        this.setSkin(skin);
      }
    }
    if (compoundTag.contains(DATA_SKIN_URL_TAG)) {
      String url = compoundTag.getString(DATA_SKIN_URL_TAG);
      if (url != null && !url.isEmpty()) {
        this.setSkinURL(url);
      }
    }
    if (compoundTag.contains(DATA_SKIN_UUID_TAG)) {
      UUID skinUUID = compoundTag.getUUID(DATA_SKIN_UUID_TAG);
      if (skinUUID != null) {
        this.setSkinUUID(skinUUID);
      }
    }
    if (compoundTag.contains(DATA_SKIN_TYPE_TAG)) {
      String skinType = compoundTag.getString(DATA_SKIN_TYPE_TAG);
      if (skinType != null && !skinType.isEmpty()) {
        this.setSkinType(this.getSkinType(skinType));
      }
    }
    if (compoundTag.contains(DATA_INVENTORY_TAG)) {
      this.inventory.fromTag(compoundTag.getList(DATA_INVENTORY_TAG, 10));
    }
  }

  @Override
  public AgeableMob getBreedOffspring(ServerLevel serverLevel, AgeableMob ageableMob) {
    return null;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityGuiScaling() {
    return 30;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityGuiTop() {
    return 0;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityDialogTop() {
    return 0;
  }

  @SuppressWarnings("java:S3400")
  public int getEntityDialogScaling() {
    return 50;
  }

}
