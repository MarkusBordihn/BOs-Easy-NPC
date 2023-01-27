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

import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import javax.annotation.Nullable;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.Util;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.dialog.DialogType;

public class EasyNPCEntityData extends AbstractVillager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Default values
  private ResourceLocation baseTextureLocation;
  private ResourceLocation professionTextureLocation;
  private ResourceLocation textureLocation;
  private boolean hasBaseTextureLocation = false;
  private boolean hasProfessionTextureLocation = false;
  private boolean hasTextureLocation = false;

  // Default Professions
  private static final Map<Profession, ResourceLocation> TEXTURE_BY_PROFESSION =
      Util.make(new EnumMap<>(Profession.class), map -> {
        map.put(Profession.NONE, new ResourceLocation(Constants.MOD_ID, "textures/entity/blank.png"));
      });

  // Default Variants
  private enum Variant {
    STEVE
  }

  private static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(new EnumMap<>(Variant.class), map -> {
        map.put(Variant.STEVE, new ResourceLocation("textures/entity/steve.png"));
      });

  // Synced Data
  private static final EntityDataAccessor<String> DATA_DIALOG_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_DIALOG =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  protected static final EntityDataAccessor<Optional<UUID>> DATA_OWNER_UUID_ID =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.OPTIONAL_UUID);
  private static final EntityDataAccessor<String> DATA_PROFESSION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  private static final EntityDataAccessor<String> DATA_VARIANT =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);

  // Stored Entity Data Tags
  private static final String DATA_OWNER_TAG = "Owner";
  private static final String DATA_DIALOG_TYPE_TAG = "DialogType";
  private static final String DATA_DIALOG_TAG = "Dialog";
  private static final String DATA_PROFESSION_TAG = "Profession";
  private static final String DATA_VARIANT_TAG = "Variant";

  // Temporary stats
  private Enum<?> lastProfession = Profession.NONE;
  private Enum<?> lastVariant = Variant.STEVE;

  public EasyNPCEntityData(EntityType<? extends AbstractVillager> entityType, Level level) {
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

  public ResourceLocation getTextureLocation(Enum<?> variant) {
    return TEXTURE_BY_VARIANT.get(variant);
  }

  public void setTextureLocation(ResourceLocation textureLocation) {
    this.textureLocation = textureLocation;
    this.hasTextureLocation = textureLocation != null;
  }

  public void setTextureLocation(Enum<?> variant) {
    ResourceLocation resourceLocation = getTextureLocation(variant);
    if (resourceLocation != null) {
      setTextureLocation(resourceLocation);
    } else {
      log.error("Unknown texture {} for variant {} and {}", resourceLocation, variant, this);
    }
  }

  public boolean hasProfessionTextureLocation() {
    return this.hasProfessionTextureLocation;
  }

  public ResourceLocation getProfessionTextureLocation() {
    return this.professionTextureLocation;
  }

  public ResourceLocation getProfessionTextureLocation(Enum<?> profession) {
    return TEXTURE_BY_PROFESSION.get(profession);
  }

  public void setProfessionTextureLocation(ResourceLocation textureLocation) {
    this.professionTextureLocation = textureLocation;
    this.hasProfessionTextureLocation = textureLocation != null;
  }

  public void setProfessionTextureLocation(Enum<?> profession) {
    ResourceLocation resourceLocation = getProfessionTextureLocation(profession);
    if (resourceLocation != null) {
      setProfessionTextureLocation(resourceLocation);
    } else {
      log.error("Unknown texture {} for profession {} and {}", resourceLocation, profession, this);
    }
  }

  public DialogType getDialogType() {
    return DialogType.valueOf(this.entityData.get(DATA_DIALOG_TYPE));
  }

  public void setDialogType(DialogType dialogType) {
    this.entityData.set(DATA_DIALOG_TYPE, dialogType.name());
  }

  public String getDialog() {
    return this.entityData.get(DATA_DIALOG);
  }

  public TextComponent getDialogComponent() {
    return new TextComponent(this.entityData.get(DATA_DIALOG));
  }

  public void setDialog(String dialog) {
    this.entityData.set(DATA_DIALOG, dialog);
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
    if (!this.lastProfession.equals(profession)) {
      this.entityData.set(DATA_PROFESSION, profession.name());
      this.setProfessionTextureLocation(profession);
      this.lastProfession = profession;
    }
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

  public boolean hasChangedProfession() {
    return this.lastProfession != null
        && !this.lastProfession.name().equals(getProfession().name());
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
    if (!this.lastVariant.equals(variant)) {
      this.entityData.set(DATA_VARIANT, variant.name());
      this.setTextureLocation(variant);
      this.lastVariant = variant;
    }
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

  public boolean hasChangedVariant() {
    return this.lastVariant != null && !this.lastVariant.name().equals(getVariant().name());
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
    this.entityData.define(DATA_DIALOG_TYPE, DialogType.NONE.name());
    this.entityData.define(DATA_DIALOG, "");
    this.entityData.define(DATA_OWNER_UUID_ID, Optional.empty());
    this.entityData.define(DATA_PROFESSION, this.getDefaultProfession().name());
    this.entityData.define(DATA_VARIANT, this.getDefaultVariant().name());
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
    if (this.getOwnerUUID() != null) {
      compoundTag.putUUID(DATA_OWNER_TAG, this.getOwnerUUID());
    }
    if (this.getProfession() != null) {
      compoundTag.putString(DATA_PROFESSION_TAG, this.getProfession().name());
    }
    if (this.getVariant() != null) {
      compoundTag.putString(DATA_VARIANT_TAG, this.getVariant().name());
    }
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);

    if (compoundTag.contains(DATA_DIALOG_TYPE_TAG)) {
      String dialogType = compoundTag.getString(DATA_DIALOG_TYPE_TAG);
      if (dialogType != null && !dialogType.isEmpty()) {
        this.setDialogType(DialogType.valueOf(dialogType));
      }
    }
    if (compoundTag.contains(DATA_DIALOG_TAG)) {
      String dialog = compoundTag.getString(DATA_DIALOG_TAG);
      if (dialog != null) {
        this.setDialog(dialog);
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

  }

  @Override
  protected void rewardTradeXp(MerchantOffer merchantOffer) {
    // Unused
  }

  @Override
  protected void updateTrades() {
    // Unused
  }

  @Override
  public AgeableMob getBreedOffspring(ServerLevel serverLevel, AgeableMob ageableMob) {
    return null;
  }

}
