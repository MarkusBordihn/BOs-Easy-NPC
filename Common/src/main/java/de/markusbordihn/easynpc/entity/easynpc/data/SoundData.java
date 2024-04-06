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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.sound.SoundDataEntry;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

public interface SoundData<E extends PathfinderMob> extends EasyNPC<E> {

  EntityDataSerializer<SoundDataSet> SOUND_DATA_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, SoundDataSet value) {
          buffer.writeNbt(value.createTag());
        }

        public SoundDataSet read(FriendlyByteBuf buffer) {
          return new SoundDataSet(buffer.readNbt());
        }

        public SoundDataSet copy(SoundDataSet value) {
          return value;
        }
      };

  EntityDataAccessor<SoundDataSet> EASY_NPC_DATA_SOUND_DATA_SET =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SOUND_DATA_SET);
  String EASY_NPC_DATA_SOUND_DATA_TAG = "SoundData";

  static void registerSoundDataSerializer() {
    EntityDataSerializers.registerSerializer(SOUND_DATA_SET);
  }

  default SoundDataSet getSoundDataSet() {
    return getEasyNPCData(EASY_NPC_DATA_SOUND_DATA_SET);
  }

  default void setSoundDataSet(SoundDataSet soundDataSet) {
    setEasyNPCData(EASY_NPC_DATA_SOUND_DATA_SET, soundDataSet);
  }

  default SoundDataSet getDefaultSoundDataSet(SoundDataSet soundDataSet, String variantName) {
    return soundDataSet;
  }

  default void clearSoundDataSet() {
    setSoundDataSet(new SoundDataSet());
  }

  default boolean hasDefaultSound(SoundType soundType) {
    return this.getSoundDataSet().hasSound(soundType);
  }

  default SoundDataEntry getDefaultSound(SoundType soundType) {
    return this.getSoundDataSet().getSound(soundType);
  }

  default SoundEvent getDefaultSoundEvent(SoundType soundType) {
    if (!this.hasDefaultSound(soundType)) {
      return null;
    }
    return this.getDefaultSound(soundType).getSoundEvent();
  }

  default void playDefaultAmbientSound() {
    OwnerData<E> ownerData = this.getEasyNPCOwnerData();
    if (ownerData != null && ownerData.hasOwner()) {
      if (hasDefaultSound(SoundType.AMBIENT_TAMED) && this.randomNumber.nextInt(4) == 0) {
        this.playDefaultSound(SoundType.AMBIENT_TAMED);
        return;
      } else if (hasDefaultSound(SoundType.AMBIENT)) {
        this.playDefaultSound(SoundType.AMBIENT);
        return;
      }
    }

    if (hasDefaultSound(SoundType.AMBIENT_STRAY)) {
      this.playDefaultSound(SoundType.AMBIENT_STRAY);
    } else {
      this.playDefaultSound(SoundType.AMBIENT);
    }
  }

  default void playDefaultHurtSound(DamageSource damageSource) {
    this.playDefaultSound(SoundType.HURT);
  }

  default void playDefaultStepSound(BlockPos blockPos, BlockState blockState) {
    this.playDefaultSound(SoundType.STEP);
  }

  default void playDefaultSound(SoundType soundType) {
    Level level = this.getLevel();
    Entity entity = this.getEntity();
    if (soundType == null
        || level == null
        || entity == null
        || entity.isSilent()
        || !this.hasDefaultSound(soundType)) {
      return;
    }
    SoundDataEntry soundDataEntry = this.getDefaultSound(soundType);
    if (soundDataEntry.isEnabled() && soundDataEntry.getVolume() > 0.0F) {
      SoundEvent soundEvent = soundDataEntry.getSoundEvent();
      if (soundEvent != null) {
        entity.playSound(
            soundEvent,
            soundDataEntry.getVolume(),
            soundDataEntry.getPitch()
                + +(EasyNPC.randomNumber.nextFloat() - EasyNPC.randomNumber.nextFloat()) * 0.3F);
      }
    }
  }

  default SoundEvent getDefaultDeathSound() {
    return this.getDefaultSoundEvent(SoundType.DEATH);
  }

  default void defineSynchedSoundData() {
    defineEasyNPCData(EASY_NPC_DATA_SOUND_DATA_SET, new SoundDataSet());
  }

  default void registerDefaultSoundData(Enum<?> variant) {
    SoundDataSet soundDataSet = this.getSoundDataSet();
    if (soundDataSet == null || soundDataSet.isEmpty()) {
      this.setSoundDataSet(this.getDefaultSoundDataSet(new SoundDataSet(), variant.name()));
    }
  }

  default void addAdditionalSoundData(CompoundTag compoundTag) {
    CompoundTag soundDataTag = new CompoundTag();

    SoundDataSet soundDataSet = this.getSoundDataSet();
    if (soundDataSet != null && !soundDataSet.isEmpty()) {
      soundDataSet.save(soundDataTag);
    } else {
      VariantData<E> variantData = this.getEasyNPCVariantData();
      SoundDataSet defaultSoundDataSet =
          this.getDefaultSoundDataSet(
              new SoundDataSet(), variantData != null ? variantData.getVariant().name() : "");
      defaultSoundDataSet.save(soundDataTag);
    }

    compoundTag.put(EASY_NPC_DATA_SOUND_DATA_TAG, soundDataTag);
  }

  default void readAdditionalSoundData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_SOUND_DATA_TAG)) {
      return;
    }

    CompoundTag soundDataTag = compoundTag.getCompound(EASY_NPC_DATA_SOUND_DATA_TAG);

    if (soundDataTag.contains(SoundDataSet.DATA_SOUND_DATA_SET_TAG)) {
      SoundDataSet soundDataSet = new SoundDataSet(soundDataTag);
      this.setSoundDataSet(soundDataSet);
    }
  }
}
