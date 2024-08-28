package de.markusbordihn.easynpc.data.skin;

import de.markusbordihn.easynpc.Constants;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record SkinDataEntry(
    String name, String url, UUID uuid, SkinType type, String content, long timestamp) {

  static final String DATA_NAME_TAG = "Name";
  static final String DATA_TYPE_TAG = "Type";
  static final String DATA_URL_TAG = "URL";
  static final String DATA_UUID_TAG = "UUID";
  static final String DATA_CONTENT_TAG = "Content";
  static final String DATA_TIMESTAMP_TAG = "Timestamp";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public SkinDataEntry() {
    this("", "", Constants.BLANK_UUID, SkinType.DEFAULT, "", System.currentTimeMillis());
  }

  public SkinDataEntry(final String name, final String url, final UUID uuid, final SkinType type) {
    this(name, url, uuid, type, "", System.currentTimeMillis());
  }

  public SkinDataEntry(final CompoundTag compoundTag) {
    this(
        compoundTag.contains(DATA_NAME_TAG) ? compoundTag.getString(DATA_NAME_TAG) : "",
        compoundTag.contains(DATA_URL_TAG) ? compoundTag.getString(DATA_URL_TAG) : "",
        compoundTag.contains(DATA_UUID_TAG)
            ? compoundTag.getUUID(DATA_UUID_TAG)
            : Constants.BLANK_UUID,
        SkinType.get(compoundTag.getString(DATA_TYPE_TAG)),
        compoundTag.contains(DATA_CONTENT_TAG) ? compoundTag.getString(DATA_CONTENT_TAG) : "",
        compoundTag.contains(DATA_TIMESTAMP_TAG)
            ? compoundTag.getLong(DATA_TIMESTAMP_TAG)
            : System.currentTimeMillis());
  }

  public SkinDataEntry withName(final String name) {
    return new SkinDataEntry(name, this.url, this.uuid, this.type, this.content, this.timestamp);
  }

  public SkinDataEntry withType(final SkinType type) {
    return new SkinDataEntry(this.name, this.url, this.uuid, type, this.content, this.timestamp);
  }

  public SkinDataEntry withURL(final String url) {
    return new SkinDataEntry(this.name, url, this.uuid, this.type, this.content, this.timestamp);
  }

  public SkinDataEntry withUUID(final UUID uuid) {
    return new SkinDataEntry(this.name, this.url, uuid, this.type, this.content, this.timestamp);
  }

  public SkinDataEntry create(CompoundTag compoundTag) {
    return new SkinDataEntry(compoundTag);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putString(DATA_NAME_TAG, this.name);
    compoundTag.putString(DATA_TYPE_TAG, this.type.name());
    compoundTag.putString(DATA_URL_TAG, this.url);
    compoundTag.putUUID(DATA_UUID_TAG, this.uuid);
    compoundTag.putString(DATA_CONTENT_TAG, this.content);
    compoundTag.putLong(DATA_TIMESTAMP_TAG, this.timestamp);
    return compoundTag;
  }

  public CompoundTag createTag() {
    return write(new CompoundTag());
  }

  @Override
  public boolean equals(Object object) {
    if (object instanceof SkinDataEntry skinDataEntry) {
      return this.name.equals(skinDataEntry.name)
          && this.type.equals(skinDataEntry.type)
          && this.url.equals(skinDataEntry.url)
          && this.uuid.equals(skinDataEntry.uuid)
          && this.content.equals(skinDataEntry.content)
          && this.timestamp == skinDataEntry.timestamp;
    }
    return false;
  }

  @Override
  public int hashCode() {
    int result = 16;
    result = 31 * result + this.name.hashCode();
    result = 31 * result + this.type.hashCode();
    result = 31 * result + this.url.hashCode();
    result = 31 * result + this.uuid.hashCode();
    if (!this.content.isEmpty()) {
      result = 31 * result + this.content.hashCode();
    }
    if (this.timestamp != 0) {
      result = 31 * result + Long.hashCode(this.timestamp);
    }
    return result;
  }

  @Override
  public String toString() {
    return "SkinDataEntry{"
        + "name="
        + this.name
        + ", type="
        + this.type
        + ", url="
        + this.url
        + ", uuid="
        + this.uuid
        + ", content="
        + this.content
        + ", timestamp="
        + this.timestamp
        + '}';
  }
}
