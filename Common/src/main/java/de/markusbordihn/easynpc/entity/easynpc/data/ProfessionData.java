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

import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface ProfessionData<T extends PathfinderMob> extends EasyNPC<T> {

  String DATA_PROFESSION_TAG = "Profession";

  static void registerSyncedProfessionData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Profession Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.PROFESSION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.PROFESSION));
  }

  default Profession getDefaultProfession() {
    return Profession.NONE;
  }

  default Profession getProfession() {
    return getSynchedEntityData(SynchedDataIndex.PROFESSION);
  }

  default void setProfession(Profession profession) {
    setSynchedEntityData(SynchedDataIndex.PROFESSION, profession);
  }

  default void setProfession(String name) {
    Profession profession = getProfession(name);
    if (profession != null) {
      setProfession(profession);
    } else {
      EasyNPC.log.error("Unknown profession {} for {}", name, this);
    }
  }

  default Profession getProfession(String name) {
    return Profession.valueOf(name);
  }

  default boolean hasProfessions() {
    return false;
  }

  default Profession[] getProfessions() {
    return Profession.values();
  }

  default boolean hasProfession() {
    return false;
  }

  default Component getProfessionName() {
    Enum<?> profession = getProfession();
    return profession != null ? TextUtils.normalizeName(profession.name()) : Component.literal("");
  }

  default void defineSynchedProfessionData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.PROFESSION, getDefaultProfession());
  }

  default void addAdditionalProfessionData(CompoundTag compoundTag) {
    if (this.getProfession() != null) {
      compoundTag.putString(DATA_PROFESSION_TAG, this.getProfession().name());
    }
  }

  default void readAdditionalProfessionData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_PROFESSION_TAG)) {
      String profession = compoundTag.getString(DATA_PROFESSION_TAG);
      if (!profession.isEmpty()) {
        this.setProfession(this.getProfession(profession));
      }
    }
  }
}
